{-| This module contains operations that output modified nodes.
    These functions use reading, deconstructing and building APIs.

-}

module Empire.ASTOps.Modify where

import           Control.Monad (forM)
import           Data.List     (find)

import           Empire.Prelude hiding (from, substitute, to)
import qualified Empire.Prelude as P

import qualified Data.Graph.Data.Component.Vector   as PtrList
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
import           Data.Text.Span                     (SpacedSpan(..))
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)
import qualified Luna.IR                            as IR

import qualified Data.List as L (take, head, drop, tail)



addLambdaArg :: Int -> NodeRef -> Maybe Text -> [String] -> GraphOp ()
addLambdaArg position lambda name varNames = do
    argNames <- getArgNames lambda
    let forbidden = varNames <> argNames
    let Just placeholderName = find (`notElem` forbidden) allWords
    let nameForNewArg = case name of
            Just n -> if convert n `elem` forbidden then placeholderName else convert n
            _      -> placeholderName
    match lambda $ \case
        Lam _arg _body -> do
            addLambdaArg' position nameForNewArg Nothing lambda
        Grouped g -> source g >>= \k -> addLambdaArg position k name varNames
        ASGFunction n as' _ -> do
            as <- ptrListToList as'
            let argsBefore        = take position as
                argsAfter         = drop position as
            v <- IR.var' $ convert nameForNewArg
            putLayer @SpanLength v (convert $ length nameForNewArg)
            l <- link v lambda
            putLayer @SpanOffset l 1
            insertPosition <- do
                let lastEdgeBeforeArg = case argsBefore of
                        [] -> coerce n
                        a  -> unsafeLast a
                Just funBeg <- Code.getOffsetRelativeToFile lambda
                lastOff <- Code.getOffsetRelativeToTarget $ coerce lastEdgeBeforeArg
                lastLen <- getLayer @SpanLength =<< source (coerce lastEdgeBeforeArg)
                return $ funBeg + lastOff + lastLen
            Code.insertAt insertPosition (" " <> convert nameForNewArg)
            Just (lam' :: Expr (P.ASGFunction)) <- narrowTerm lambda
            ptrList <- PtrList.fromList $ coerce $ argsBefore <> (l : argsAfter)
            IR.UniTermFunction a <- getLayer @IR.Model lam'
            let a' = a & IR.args_Function .~ ptrList
            putLayer @IR.Model lam' $ IR.UniTermFunction a'
            -- IR.modifyExprTerm lam' $ wrapped . IR.termASGFunction_args .~ fmap coerce (argsBefore <> (l : argsAfter))
            Code.gossipUsesChangedBy (1 + fromIntegral (length nameForNewArg)) v
        _ -> throwM $ NotLambdaException lambda

allWords :: [String]
allWords = drop 1 $ allWords' where
    allWords' = fmap reverse $ "" : (flip (:) <$> allWords' <*> ['a' .. 'z'])

getArgNames :: NodeRef -> GraphOp [String]
getArgNames ref = match ref $ \case
    Grouped g   -> source g >>= getArgNames
    Lam a body -> do
        argNames <- ASTRead.getPatternNames =<< source a
        (argNames <>) <$> (getArgNames =<< source body)
    ASGFunction _ as' _ -> do
        as <- ptrListToList as'
        concat <$> mapM (ASTRead.getPatternNames <=< source) as
    _ -> return []

replaceWithLam :: Maybe EdgeRef -> String -> NodeRef -> GraphOp ()
replaceWithLam parent name lam = do
    tmpBlank    <- IR.blank
    binder      <- IR.var $ stringToName name
    let argLen = fromIntegral $ length name
    putLayer @SpanLength binder argLen
    newLam      <- generalize <$> IR.lam binder tmpBlank
    putLayer @SpanLength newLam $ argLen + 2 + 1 -- ": " + "_"
    matchExpr newLam $ \case
        Lam _ b -> putLayer @SpanOffset b 2 -- ": "
    lamIsLambda <- ASTRead.isLambda lam
    if lamIsLambda then do
        Just beg <- Code.getAnyBeginningOf lam
        Code.applyDiff beg beg $ convert $ name <> ": "
    else do
        Just beg <- join <$> forM parent (\prevLam -> target prevLam >>= flip matchExpr `id` \case
            Lam arg _ -> do
                o   <- getLayer @SpanLength =<< source arg
                beg <- Code.getAnyBeginningOf =<< target prevLam
                return $ fmap (+ o) beg)
        Code.applyDiff beg beg $ convert $ ": " <> name
    case parent of
        Just e  -> do
            oldEdgeOffset <- getLayer @SpanOffset e
            matchExpr newLam $ \case
                Lam _ b -> putLayer @SpanOffset b oldEdgeOffset
            putLayer @SpanOffset e 2
            replaceSource newLam $ coerce e
        Nothing -> substitute newLam lam
    P.replace lam tmpBlank
    Code.gossipLengthsChangedBy (argLen + 2) $
        if isJust parent then newLam else lam
    return ()

addLambdaArg' :: Int -> String -> Maybe EdgeRef -> NodeRef -> GraphOp ()
addLambdaArg' 0   name parent lam = replaceWithLam parent name lam
addLambdaArg' pos name parent lam = match lam $ \case
    Lam _ b -> addLambdaArg' (pos - 1) name (Just $ coerce b) =<< source b
    _       -> replaceWithLam parent name lam

data CannotRemovePortException = CannotRemovePortException
    deriving Show

instance Exception CannotRemovePortException where
    toException = astExceptionToException
    fromException = astExceptionFromException

lamAny :: NodeRef -> NodeRef -> GraphOp NodeRef
lamAny a b = fmap generalize $ IR.lam a b

lams :: [NodeRef] -> NodeRef -> GraphOp NodeRef
lams args output = unsafeRelayout <$> foldM (flip lamAny) (unsafeRelayout output) (unsafeRelayout <$> reverse args)

removeLambdaArg' :: Int -> NodeRef -> Maybe EdgeRef -> GraphOp ()
removeLambdaArg' 0 ref Nothing = match ref $ \case
    Lam _ b -> do
        body <- source b
        nextIsLam <- ASTRead.isLambda body
        if nextIsLam then do
            Just lamBeg  <- Code.getAnyBeginningOf ref
            Just nextBeg <- Code.getAnyBeginningOf body
            Code.applyDiff lamBeg nextBeg ""
            P.replace body ref
            Code.gossipLengthsChangedBy (lamBeg - nextBeg) body
        else throwM CannotRemovePortException
removeLambdaArg' 0 ref (Just parent) = matchExpr ref $ \case
    Lam arg b -> do
        Just lamBeg  <- Code.getAnyBeginningOf ref
        body         <- source b
        argLen       <- length <$> (ASTRead.getVarName =<< source arg)
        off          <- getLayer @SpanOffset parent
        Code.applyDiff (lamBeg - off) (lamBeg + convert argLen) ""
        P.replace body ref
        parent' <- target parent
        Code.gossipLengthsChangedBy (negate $ off + convert argLen) parent'
    _ -> throwM CannotRemovePortException
removeLambdaArg' port ref _ = match ref $ \case
    Lam _ b -> do
        body <- source b
        removeLambdaArg' (port - 1) body (Just $ coerce b)
    _ -> throwM CannotRemovePortException

removeLambdaArg :: Port.OutPortId -> NodeRef -> GraphOp ()
removeLambdaArg [] _ = throwM $ CannotRemovePortException
removeLambdaArg p@(Port.Projection port : []) lambda = match lambda $ \case
    Grouped g      -> source g >>= removeLambdaArg p
    Lam _arg _body -> removeLambdaArg' port lambda Nothing
    ASGFunction _ as' _ -> do
        as <- ptrListToList as'
        let argsBefore        = take port       as
            argsAfter         = drop (port + 1) as
            argToRemove       = as ^? ix port
        for_ argToRemove $ \alink -> do
            Just funBeg <- Code.getOffsetRelativeToFile lambda
            offToLam    <- Code.getOffsetRelativeToTarget $ coerce alink
            ownOff      <- getLayer @SpanOffset alink
            ownLen      <- getLayer @SpanLength =<< source alink
            Code.removeAt (funBeg + offToLam - ownOff) (funBeg + offToLam + ownLen)
            Just (lam' :: Expr (P.ASGFunction)) <- narrowTerm lambda
            l <- PtrList.fromList $ coerce $ argsBefore <> argsAfter
            IR.UniTermFunction a <- getLayer @IR.Model lam'
            let a' = a & IR.args_Function .~ l
            putLayer @IR.Model lam' $ IR.UniTermFunction a'
            -- IR.modifyExprTerm lam' $ wrapped . IR.termASGFunction_args .~ fmap coerce (argsBefore <> argsAfter)
            arg <- source alink
            irDeleteLink alink
            deleteSubtree arg
            Code.gossipLengthsChangedBy (-(ownOff + ownLen)) lambda
    _ -> throwM $ NotLambdaException lambda
removeLambdaArg _ _ = throwM CannotRemovePortException

shiftPosition :: Int -> Int -> [a] -> [a]
shiftPosition from to lst = uncurry (insertAt to) $ getAndRemove from lst where
    insertAt 0 e l        = e : l
    insertAt i e (x : xs) = x : insertAt (i - 1) e xs
    insertAt _ _ []       = error "shiftPosition: insertAt: empty list"

    getAndRemove 0 (x : xs) = (x, xs)
    getAndRemove i (x : xs) = let (r, rs) = getAndRemove (i - 1) xs in (r, x : rs)
    getAndRemove _ []       = error "shiftPosition: getAndRemove: empty list"

swapLamVars :: Delta -> (Delta, EdgeRef) -> (Delta, EdgeRef) -> GraphOp Int
swapLamVars lamBeg one two = do
    one'      <- source $ snd one
    two'      <- source $ snd two
    oneLength <- getLayer @SpanLength one'
    twoLength <- getLayer @SpanLength two'
    oneCode   <- Code.getAt (lamBeg + fst one) (lamBeg + fst one + oneLength)
    twoCode   <- Code.getAt (lamBeg + fst two) (lamBeg + fst two + twoLength)
    Code.applyMany [ (lamBeg + fst one, lamBeg + fst one + oneLength, twoCode)
                   , (lamBeg + fst two, lamBeg + fst two + twoLength, oneCode)
                   ]
    replaceSource two' $ coerce (snd one)
    replaceSource one' $ coerce (snd two)
    oneAfter' <- source $ snd one
    twoAfter' <- source $ snd two
    putLayer @SpanLength oneAfter' twoLength
    putLayer @SpanLength twoAfter' oneLength
    let change = if fst one > fst two then fromIntegral oneLength - fromIntegral twoLength else fromIntegral twoLength - fromIntegral oneLength
    return change

moveLambdaArg :: Port.OutPortId -> Int -> NodeRef -> GraphOp ()
moveLambdaArg [] _ _ = throwM $ CannotRemovePortException
moveLambdaArg p@(Port.Projection port : []) newPosition lambda = match lambda $ \case
    Grouped g -> source g >>= moveLambdaArg p newPosition
    Lam _ _   -> do
        Just lamBeg   <- Code.getOffsetRelativeToFile lambda
        args <- ASTDeconstruct.extractLamArgLinks lambda
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
    ASGFunction _ as' _ -> do
        as <- ptrListToList as'
        for_ (as ^? ix port) $ \alink -> do
            Just funBeg   <- Code.getOffsetRelativeToFile   lambda
            initialOffset <- (+(funBeg)) <$> Code.getOffsetRelativeToTarget (coerce alink)
            let newArgs = shiftPosition port newPosition as
            Just (lam' :: Expr (P.ASGFunction)) <- narrowTerm lambda
            l <- PtrList.fromList $ coerce newArgs
            IR.UniTermFunction a <- getLayer @IR.Model lam'
            let a' = a & IR.args_Function .~ l
            putLayer @IR.Model lam' $ IR.UniTermFunction a'
            -- IR.modifyExprTerm lam' $ wrapped . IR.termASGFunction_args .~ fmap coerce newArgs
            newOffset <- (+(funBeg)) <$> Code.getOffsetRelativeToTarget (coerce alink)
            ownOff    <- getLayer @SpanOffset alink
            ownLen    <- getLayer @SpanLength =<< source alink
            _landingLen <- do
                let landingVar = as ^? ix newPosition
                landingLenMay <- for landingVar (\l' -> source l' >>= getLayer @SpanLength)
                maybe (throwM $ PortDoesNotExistException [Port.Projection newPosition]) pure landingLenMay
            code      <- Code.getAt (initialOffset - ownOff) (initialOffset + ownLen)
            Code.applyDiff (initialOffset - ownOff) (initialOffset + ownLen) ""
            Code.applyDiff (newOffset     - ownOff) (newOffset - ownOff) code
    _ -> throwM $ NotLambdaException lambda
moveLambdaArg _ _ _ = throwM CannotRemovePortException

renameVarAndReplaceInCode :: NodeRef -> String -> GraphOp ()
renameVarAndReplaceInCode var newName = do
    oldLen <- getLayer @SpanLength var
    renameVar var newName
    Code.replaceAllUses var oldLen $ convert newName

renameLambdaArg :: Port.OutPortId -> String -> NodeRef -> GraphOp ()
renameLambdaArg [] _ _ = throwM CannotRemovePortException
renameLambdaArg p@(Port.Projection port : []) newName lam = match lam $ \case
    Grouped g -> source g >>= renameLambdaArg p newName
    Lam _ _ -> do
        args <- ASTDeconstruct.extractArguments lam
        let arg = args !! port
        renameVarAndReplaceInCode arg newName
    ASGFunction _ as' _ -> do
        as <- ptrListToList as'
        for_ (as ^? ix port) $ \alink -> do
            arg <- source alink
            renameVarAndReplaceInCode arg newName
    _ -> throwM $ NotLambdaException lam
renameLambdaArg _ _ _ = throwM CannotRemovePortException

redirectLambdaOutput :: NodeRef -> NodeRef -> GraphOp NodeRef
redirectLambdaOutput lambda newOutputRef = do
    match lambda $ \case
        Grouped g   -> source g >>= flip redirectLambdaOutput newOutputRef >>= fmap generalize . IR.grouped
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            lams args' newOutputRef
        _ -> throwM $ NotLambdaException lambda

setLambdaOutputToBlank :: NodeRef -> GraphOp NodeRef
setLambdaOutputToBlank lambda = do
    match lambda $ \case
        Grouped g   -> source g >>= setLambdaOutputToBlank >>= fmap generalize . IR.grouped
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            blank <- generalize <$> IR.blank
            lams args' blank
        _ -> throwM $ NotLambdaException lambda

replaceTargetNode :: NodeRef -> NodeRef -> GraphOp ()
replaceTargetNode matchNode newTarget = do
    match matchNode $ \case
        Unify _l r -> do
            replaceSource newTarget $ coerce r
        _ -> throwM $ NotUnifyException matchNode

replaceVarNode :: NodeRef -> NodeRef -> GraphOp ()
replaceVarNode matchNode newVar = do
    match matchNode $ \case
        Unify l _r -> do
            replaceSource newVar $ coerce l
        _ -> throwM $ NotUnifyException matchNode

rewireNode :: NodeId -> NodeRef -> GraphOp ()
rewireNode nodeId newTarget = do
    ref <- ASTRead.getASTPointer nodeId
    match ref $ \case
        Unify{} -> do
            oldTarget <- ASTRead.getASTTarget  nodeId
            replaceTargetNode ref newTarget
            ASTRemove.removeSubtree oldTarget
        _ -> do
            pointer <- ASTRead.getASTPointer nodeId
            P.replace newTarget pointer

rewireNodeName :: NodeId -> NodeRef -> GraphOp ()
rewireNodeName nodeId newVar = do
    matchNode <- ASTRead.getASTPointer nodeId
    oldVar    <- ASTRead.getASTVar  nodeId
    replaceVarNode matchNode newVar
    ASTRemove.removeSubtree oldVar

rewireCurrentNode :: NodeRef -> GraphOp ()
rewireCurrentNode newTarget = do
    matchNode <- ASTRead.getCurrentASTPointer
    oldTarget <- ASTRead.getCurrentASTTarget
    replaceTargetNode matchNode newTarget
    ASTRemove.removeSubtree oldTarget

renameVar :: NodeRef -> String -> ASTOp g ()
renameVar vref name = do
    var <- narrowTerm @IR.Var vref
    case var of
        Just var' -> do
            matchExpr var' $ \case
                IR.UniTermVar a -> do
                    let a' = a & IR.name_Var .~ stringToName name
                    putLayer @IR.Model var' $ IR.UniTermVar a'
                    LeftSpacedSpan (SpacedSpan off _len) <-
                        view CodeSpan.realSpan <$> getLayer @CodeSpan vref
                    putLayer @CodeSpan vref $ CodeSpan.mkRealSpan
                        (LeftSpacedSpan (SpacedSpan off (fromIntegral $ length name)))
                IR.UniTermInvalid{} -> do
                    v <- IR.var $ convert name
                    putLayer @SpanLength v $ fromIntegral $ length name
                    LeftSpacedSpan (SpacedSpan off _len) <-
                        view CodeSpan.realSpan <$> getLayer @CodeSpan vref
                    putLayer @CodeSpan v $ CodeSpan.mkRealSpan
                        (LeftSpacedSpan (SpacedSpan off (fromIntegral $ length name)))
                    IR.replace v vref
                    return ()
        _ -> return ()
    -- mapM_ (flip IR.modifyExprTerm $ IR.name .~ (stringToName name)) var

replaceWhenBHSelf :: NodeRef -> NodeRef -> GraphOp ()
replaceWhenBHSelf to from = do
    oldRef  <- use $ Graph.breadcrumbHierarchy . BH.self
    when (oldRef == from)  $ Graph.breadcrumbHierarchy . BH.self .= to

replace :: NodeRef -> NodeRef -> GraphOp ()
replace to from = do
    P.replace to from
    replaceWhenBHSelf to from

substitute :: NodeRef -> NodeRef -> GraphOp ()
substitute to from = do
    P.substitute to from
    replaceWhenBHSelf to from
