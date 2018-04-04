{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

{-| This module contains operations that output modified nodes.
    These functions use reading, deconstructing and building APIs.

-}

module Empire.ASTOps.Modify where

import           Control.Monad (forM)
import           Control.Lens  (folded, ifiltered)
import           Data.List     (find)
import qualified Data.Text.IO  as Text
import qualified Safe

import           Empire.Prelude

import           Data.Text.Position                 (Delta)
import           LunaStudio.Data.NodeId             (NodeId)
import qualified LunaStudio.Data.Port               as Port
import           Empire.ASTOp                       (GraphOp, ASTOp, match)
import qualified Empire.ASTOps.Deconstruct          as ASTDeconstruct
import qualified Empire.ASTOps.Read                 as ASTRead
import qualified Empire.ASTOps.Remove               as ASTRemove
import qualified Empire.Commands.Code               as Code
import           Empire.Data.AST                    (EdgeRef, NodeRef, NotLambdaException(..),
                                                     NotUnifyException(..), PortDoesNotExistException(..),
                                                     astExceptionToException, astExceptionFromException)
import           Empire.Data.Layers                 (SpanLength, SpanOffset)
import qualified Empire.Data.BreadcrumbHierarchy    as BH
import qualified Empire.Data.Graph                  as Graph

import qualified OCI.IR.Combinators                 as IR
import qualified Luna.IR                            as IR
import           Luna.IR.Term.Uni

import qualified Data.List as L (take, head, drop, tail)



addLambdaArg :: GraphOp m => Int -> NodeRef -> Maybe Text -> [String] -> m ()
addLambdaArg position lambda name varNames = do
    argNames <- getArgNames lambda
    let forbidden = varNames <> argNames
    let Just placeholderName = find (`notElem` forbidden) allWords
    let nameForNewArg = case name of
            Just n -> if convert n `elem` forbidden then placeholderName else convert n
            _      -> placeholderName
    match lambda $ \case
        Lam _arg _body -> do
            out'  <- ASTRead.getFirstNonLambdaRef lambda
            addLambdaArg' position nameForNewArg Nothing lambda
        Grouped g -> IR.source g >>= \k -> addLambdaArg position k name varNames
        ASGFunction n as _ -> do
            let argsBefore        = take position as
                argsAfter         = drop position as
            v <- IR.var' $ convert nameForNewArg
            IR.putLayer @SpanLength v (convert $ length nameForNewArg)
            l <- IR.unsafeGeneralize <$> IR.link v lambda
            IR.putLayer @SpanOffset l 1
            insertPosition <- do
                let lastEdgeBeforeArg = case argsBefore of
                        [] -> n
                        a  -> unsafeLast a
                Just funBeg <- Code.getOffsetRelativeToFile lambda
                lastOff <- Code.getOffsetRelativeToTarget lastEdgeBeforeArg
                lastLen <- IR.getLayer @SpanLength =<< IR.source lastEdgeBeforeArg
                return $ funBeg + lastOff + lastLen
            Code.insertAt insertPosition (" " <> convert nameForNewArg)
            Just (lam' :: IR.Expr (IR.ASGFunction)) <- IR.narrow lambda
            IR.modifyExprTerm lam' $ wrapped . IR.termASGFunction_args .~ fmap IR.unsafeGeneralize (argsBefore <> (l : argsAfter))
            Code.gossipUsesChangedBy (1 + fromIntegral (length nameForNewArg)) v
        _ -> throwM $ NotLambdaException lambda

allWords :: [String]
allWords = drop 1 $ allWords' where
    allWords' = fmap reverse $ "" : (flip (:) <$> allWords' <*> ['a' .. 'z'])

getArgNames :: GraphOp m => NodeRef -> m [String]
getArgNames ref = match ref $ \case
    Grouped g   -> IR.source g >>= getArgNames
    Lam a body -> do
        argNames <- ASTRead.getPatternNames =<< IR.source a
        (argNames <>) <$> (getArgNames =<< IR.source body)
    ASGFunction _ as _ -> concat <$> mapM (ASTRead.getPatternNames <=< IR.source) as
    _ -> return []

replaceWithLam :: GraphOp m => Maybe EdgeRef -> String -> NodeRef -> m ()
replaceWithLam parent name lam = do
    tmpBlank    <- IR.blank
    binder      <- IR.var $ stringToName name
    let argLen = fromIntegral $ length name
    IR.putLayer @SpanLength binder argLen
    newLam      <- IR.generalize <$> IR.lam binder tmpBlank
    IR.putLayer @SpanLength newLam $ argLen + 2 + 1 -- ": " + "_"
    IR.matchExpr newLam $ \case
        Lam _ b -> IR.putLayer @SpanOffset b 2 -- ": "
    lamIsLambda <- ASTRead.isLambda lam
    if lamIsLambda then do
        Just beg <- Code.getAnyBeginningOf lam
        Code.applyDiff beg beg $ convert $ name <> ": "
    else do
        Just beg <- join <$> forM parent (\prevLam -> IR.readTarget prevLam >>= flip IR.matchExpr `id` \case
            Lam arg _ -> do
                o   <- IR.getLayer @SpanLength =<< IR.source arg
                beg <- Code.getAnyBeginningOf =<< IR.readTarget prevLam
                return $ fmap (+ o) beg)
        Code.applyDiff beg beg $ convert $ ": " <> name
    case parent of
        Just e  -> do
            oldEdgeOffset <- IR.getLayer @SpanOffset e
            IR.matchExpr newLam $ \case
                Lam _ b -> IR.putLayer @SpanOffset b oldEdgeOffset
            IR.putLayer @SpanOffset e 2
            IR.replaceSource newLam e
        Nothing -> substitute newLam lam
    IR.replace lam tmpBlank
    Code.gossipLengthsChangedBy (argLen + 2) $ case parent of
        Just e -> newLam
        _      -> lam
    return ()

addLambdaArg' :: GraphOp m => Int -> String -> Maybe EdgeRef -> NodeRef -> m ()
addLambdaArg' 0   name parent lam = replaceWithLam parent name lam
addLambdaArg' pos name parent lam = match lam $ \case
    Lam _ b -> addLambdaArg' (pos - 1) name (Just b) =<< IR.source b
    _       -> replaceWithLam parent name lam

data CannotRemovePortException = CannotRemovePortException
    deriving Show

instance Exception CannotRemovePortException where
    toException = astExceptionToException
    fromException = astExceptionFromException

lamAny :: GraphOp m => NodeRef -> NodeRef -> m NodeRef
lamAny a b = fmap IR.generalize $ IR.lam a b

lams :: GraphOp m => [NodeRef] -> NodeRef -> m NodeRef
lams args output = IR.unsafeRelayout <$> foldM (flip lamAny) (IR.unsafeRelayout output) (IR.unsafeRelayout <$> reverse args)

removeLambdaArg' :: GraphOp m => Int -> NodeRef -> Maybe EdgeRef -> m ()
removeLambdaArg' 0 ref Nothing = match ref $ \case
    Lam _ b -> do
        body <- IR.source b
        nextIsLam <- ASTRead.isLambda body
        if nextIsLam then do
            Just lamBeg  <- Code.getAnyBeginningOf ref
            Just nextBeg <- Code.getAnyBeginningOf body
            Code.applyDiff lamBeg nextBeg ""
            IR.replace body ref
            Code.gossipLengthsChangedBy (lamBeg - nextBeg) body
        else throwM CannotRemovePortException
removeLambdaArg' 0 ref (Just parent) = match ref $ \case
    Lam arg b -> do
        Just lamBeg  <- Code.getAnyBeginningOf ref
        body         <- IR.source b
        argLen       <- length <$> (ASTRead.getVarName =<< IR.source arg)
        off          <- IR.getLayer @SpanOffset parent
        Code.applyDiff (lamBeg - off) (lamBeg + convert argLen) ""
        IR.replace body ref
        parent' <- IR.readTarget parent
        Code.gossipLengthsChangedBy (negate $ off + convert argLen) parent'
    _ -> throwM CannotRemovePortException
removeLambdaArg' port ref _ = match ref $ \case
    Lam _ b -> do
        body <- IR.source b
        removeLambdaArg' (port - 1) body (Just b)
    _ -> throwM CannotRemovePortException

removeLambdaArg :: GraphOp m => Port.OutPortId -> NodeRef -> m ()
removeLambdaArg [] _ = throwM $ CannotRemovePortException
removeLambdaArg p@(Port.Projection port : []) lambda = match lambda $ \case
    Grouped g      -> IR.source g >>= removeLambdaArg p
    Lam _arg _body -> removeLambdaArg' port lambda Nothing
    ASGFunction _ as _ -> do
        let argsBefore        = take port       as
            argsAfter         = drop (port + 1) as
            argToRemove       = as ^? ix port
        for_ argToRemove $ \alink -> do
            Just funBeg <- Code.getOffsetRelativeToFile lambda
            offToLam    <- Code.getOffsetRelativeToTarget alink
            ownOff      <- IR.getLayer @SpanOffset alink
            ownLen      <- IR.getLayer @SpanLength =<< IR.source alink
            Code.removeAt (funBeg + offToLam - ownOff) (funBeg + offToLam + ownLen)
            Just (lam' :: IR.Expr (IR.ASGFunction)) <- IR.narrow lambda
            IR.modifyExprTerm lam' $ wrapped . IR.termASGFunction_args .~ fmap IR.unsafeGeneralize (argsBefore <> argsAfter)
            arg <- IR.source alink
            IR.delete alink
            IR.deleteSubtree arg
            Code.gossipLengthsChangedBy (-(ownOff + ownLen)) lambda
    _ -> throwM $ NotLambdaException lambda

shiftPosition :: Int -> Int -> [a] -> [a]
shiftPosition from to lst = uncurry (insertAt to) $ getAndRemove from lst where
    insertAt 0 e l        = e : l
    insertAt i e (x : xs) = x : insertAt (i - 1) e xs

    getAndRemove 0 (x : xs) = (x, xs)
    getAndRemove i (x : xs) = let (r, rs) = getAndRemove (i - 1) xs in (r, x : rs)

swapLamVars :: GraphOp m => Delta -> (Delta, EdgeRef) -> (Delta, EdgeRef) -> m Int
swapLamVars lamBeg one two = do
    one'      <- IR.source $ snd one
    two'      <- IR.source $ snd two
    oneLength <- IR.getLayer @SpanLength one'
    twoLength <- IR.getLayer @SpanLength two'
    oneCode   <- Code.getAt (lamBeg + fst one) (lamBeg + fst one + oneLength)
    twoCode   <- Code.getAt (lamBeg + fst two) (lamBeg + fst two + twoLength)
    Code.applyMany [ (lamBeg + fst one, lamBeg + fst one + oneLength, twoCode)
                   , (lamBeg + fst two, lamBeg + fst two + twoLength, oneCode)
                   ]
    IR.replaceSource two' (snd one)
    IR.replaceSource one' (snd two)
    one'      <- IR.source $ snd one
    two'      <- IR.source $ snd two
    IR.putLayer @SpanLength one' twoLength
    IR.putLayer @SpanLength two' oneLength
    let change = if fst one > fst two then fromIntegral oneLength - fromIntegral twoLength else fromIntegral twoLength - fromIntegral oneLength
    return change

moveLambdaArg :: GraphOp m => Port.OutPortId -> Int -> NodeRef -> m ()
moveLambdaArg [] _ _ = throwM $ CannotRemovePortException
moveLambdaArg p@(Port.Projection port : []) newPosition lambda = match lambda $ \case
    Grouped g -> IR.source g >>= moveLambdaArg p newPosition
    Lam _ _   -> do
        Just lamBeg   <- Code.getOffsetRelativeToFile lambda
        args <- ASTDeconstruct.extractLamArgLinks lambda
        out  <- ASTRead.getFirstNonLambdaRef    lambda
        let moveStart = min port newPosition
            moveEnd   = max port newPosition
            moveRange = moveEnd - moveStart + 1
            argsMoving = L.take moveRange $ L.drop moveStart args
            h = L.head $ reverse argsMoving
            t = L.tail $ reverse argsMoving
        if (newPosition < port) then
            void $ foldM (\mRef oRef -> swapLamVars lamBeg mRef oRef >> return oRef) h t
        else
            void $ foldM (\(mRef,delta) oRef -> swapLamVars lamBeg (mRef & _1 %~ (+fromIntegral delta)) oRef >>= \d -> return (oRef, d)) (L.head argsMoving, 0) (L.tail argsMoving)
    ASGFunction _ as _ -> do
        for_ (as ^? ix port) $ \alink -> do
            Just funBeg   <- Code.getOffsetRelativeToFile   lambda
            initialOffset <- (+funBeg) <$> Code.getOffsetRelativeToTarget alink
            let newArgs = shiftPosition port newPosition as
            Just (lam' :: IR.Expr (IR.ASGFunction)) <- IR.narrow lambda
            IR.modifyExprTerm lam' $ wrapped . IR.termASGFunction_args .~ fmap IR.unsafeGeneralize newArgs
            newOffset <- (+funBeg) <$> Code.getOffsetRelativeToTarget alink
            ownOff    <- IR.getLayer @SpanOffset alink
            ownLen    <- IR.getLayer @SpanLength =<< IR.source alink
            landingLen <- do
                let landingVar = as ^? ix newPosition
                landingLenMay <- for landingVar (\l -> IR.source l >>= IR.getLayer @SpanLength)
                maybe (throwM $ PortDoesNotExistException [Port.Projection newPosition]) pure landingLenMay
            code      <- Code.getAt (initialOffset - ownOff) (initialOffset + ownLen)
            Code.applyDiff (initialOffset - ownOff) (initialOffset + ownLen) ""
            Code.applyDiff (newOffset     - ownOff) (newOffset - ownOff) code
    _ -> throwM $ NotLambdaException lambda

renameLambdaArg :: GraphOp m => Port.OutPortId -> String -> NodeRef -> m ()
renameLambdaArg [] _ _ = throwM CannotRemovePortException
renameLambdaArg p@(Port.Projection port : []) newName lam = match lam $ \case
    Grouped g -> IR.source g >>= renameLambdaArg p newName
    Lam _ _ -> do
        args <- ASTDeconstruct.extractArguments lam
        let arg = args !! port
        renameVar arg newName
        Code.replaceAllUses arg $ convert newName
    ASGFunction _ as _ -> do
        for_ (as ^? ix port) $ \alink -> do
            arg <- IR.source alink
            renameVar arg newName
            Code.replaceAllUses arg $ convert newName
    _ -> throwM $ NotLambdaException lam

redirectLambdaOutput :: GraphOp m => NodeRef -> NodeRef -> m NodeRef
redirectLambdaOutput lambda newOutputRef = do
    match lambda $ \case
        Grouped g   -> IR.source g >>= flip redirectLambdaOutput newOutputRef >>= fmap IR.generalize . IR.grouped
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            lams args' newOutputRef
        _ -> throwM $ NotLambdaException lambda

setLambdaOutputToBlank :: GraphOp m => NodeRef -> m NodeRef
setLambdaOutputToBlank lambda = do
    match lambda $ \case
        Grouped g   -> IR.source g >>= setLambdaOutputToBlank >>= fmap IR.generalize . IR.grouped
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            blank <- IR.generalize <$> IR.blank
            lams args' blank
        _ -> throwM $ NotLambdaException lambda

replaceTargetNode :: GraphOp m => NodeRef -> NodeRef -> m ()
replaceTargetNode matchNode newTarget = do
    match matchNode $ \case
        Unify _l r -> do
            IR.replaceSource newTarget r
        _ -> throwM $ NotUnifyException matchNode

replaceVarNode :: GraphOp m => NodeRef -> NodeRef -> m ()
replaceVarNode matchNode newVar = do
    match matchNode $ \case
        Unify l _r -> do
            IR.replaceSource newVar l
        _ -> throwM $ NotUnifyException matchNode

rewireNode :: GraphOp m => NodeId -> NodeRef -> m ()
rewireNode nodeId newTarget = do
    ref <- ASTRead.getASTPointer nodeId
    match ref $ \case
        IR.Unify{} -> do
            oldTarget <- ASTRead.getASTTarget  nodeId
            replaceTargetNode ref newTarget
            ASTRemove.removeSubtree oldTarget
        _ -> do
            pointer <- ASTRead.getASTPointer nodeId
            IR.replace newTarget pointer

rewireNodeName :: GraphOp m => NodeId -> NodeRef -> m ()
rewireNodeName nodeId newVar = do
    matchNode <- ASTRead.getASTPointer nodeId
    oldVar    <- ASTRead.getASTVar  nodeId
    replaceVarNode matchNode newVar
    ASTRemove.removeSubtree oldVar

rewireCurrentNode :: GraphOp m => NodeRef -> m ()
rewireCurrentNode newTarget = do
    matchNode <- ASTRead.getCurrentASTPointer
    oldTarget <- ASTRead.getCurrentASTTarget
    replaceTargetNode matchNode newTarget
    ASTRemove.removeSubtree oldTarget

renameVar :: ASTOp a m => NodeRef -> String -> m ()
renameVar vref name = do
    var <- IR.narrowTerm @IR.Var vref
    mapM_ (flip IR.modifyExprTerm $ IR.name .~ (stringToName name)) var

replaceWhenBHSelf :: GraphOp m => NodeRef -> NodeRef -> m ()
replaceWhenBHSelf to from = do
    oldRef  <- use $ Graph.breadcrumbHierarchy . BH.self
    when (oldRef == from)  $ Graph.breadcrumbHierarchy . BH.self .= to

replace :: GraphOp m => NodeRef -> NodeRef -> m ()
replace to from = do
    IR.replace to from
    replaceWhenBHSelf to from

substitute :: GraphOp m => NodeRef -> NodeRef -> m ()
substitute to from = do
    IR.substitute to from
    replaceWhenBHSelf to from
