{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-| This module contains operations that output modified nodes.
    These functions use reading, deconstructing and building APIs.

-}

module Empire.ASTOps.Modify where

import           Control.Lens (folded, ifiltered)
import           Data.List    (find)

import           Empire.Prelude

import           LunaStudio.Data.Node               (NodeId)
import qualified LunaStudio.Data.Port               as Port
import           Empire.ASTOp                       (GraphOp, ASTOp, match)
import qualified Empire.ASTOps.Deconstruct          as ASTDeconstruct
import qualified Empire.ASTOps.Read                 as ASTRead
import qualified Empire.ASTOps.Remove               as ASTRemove
import           Empire.Data.AST                    (EdgeRef, NodeRef, NotLambdaException(..),
                                                     NotUnifyException(..), astExceptionToException,
                                                     astExceptionFromException)
import qualified Empire.Data.BreadcrumbHierarchy    as BH
import qualified Empire.Data.Graph                  as Graph

import qualified OCI.IR.Combinators as IR (replaceSource, narrowTerm, replace, substitute)
import           Luna.IR.Term.Uni
import qualified Luna.IR as IR



addLambdaArg :: GraphOp m => Int -> NodeRef -> m ()
addLambdaArg position lambda = match lambda $ \case
    Lam _arg _body -> do
        out'  <- ASTRead.getFirstNonLambdaRef lambda
        names <- getArgNames lambda
        let Just nameForNewArg = find (not . flip elem names) allWords
        addLambdaArg' position nameForNewArg Nothing lambda
    Grouped g -> IR.source g >>= addLambdaArg position
    _ -> throwM $ NotLambdaException lambda

allWords :: [String]
allWords = drop 1 $ allWords' where
    allWords' = fmap reverse $ "" : (flip (:) <$> allWords' <*> ['a' .. 'z'])

getArgNames :: GraphOp m => NodeRef -> m [String]
getArgNames ref = match ref $ \case
    Grouped g   -> IR.source g >>= getArgNames
    Lam a body -> do
        argNames <- ASTRead.getPatternNames =<< IR.source a
        (argNames ++) <$> (getArgNames =<< IR.source body)
    _ -> return []

replaceWithLam :: GraphOp m => Maybe EdgeRef -> String -> NodeRef -> m ()
replaceWithLam parent name lam = do
    tmpBlank <- IR.blank
    binder   <- IR.var $ stringToName name
    newLam   <- IR.lam binder tmpBlank
    case parent of
        Just e  -> IR.replaceSource (IR.generalize newLam) e
        Nothing -> substitute (IR.generalize newLam) lam
    IR.replace lam tmpBlank
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

removeLambdaArg :: GraphOp m => Port.OutPortId -> NodeRef -> m NodeRef
removeLambdaArg [] _ = throwM $ CannotRemovePortException
removeLambdaArg p@(Port.Projection port : []) lambda = match lambda $ \case
    Grouped g      -> IR.source g >>= removeLambdaArg p >>= fmap IR.generalize . IR.grouped
    Lam _arg _body -> do
        args <- ASTDeconstruct.extractArguments lambda
        out  <- ASTRead.getFirstNonLambdaRef lambda
        let newArgs = args ^.. folded . ifiltered (\i _ -> i /= port)
        lams newArgs out
    _ -> throwM $ NotLambdaException lambda

shiftPosition :: Int -> Int -> [a] -> [a]
shiftPosition from to lst = uncurry (insertAt to) $ getAndRemove from lst where
    insertAt 0 e l        = e : l
    insertAt i e (x : xs) = x : insertAt (i - 1) e xs

    getAndRemove 0 (x : xs) = (x, xs)
    getAndRemove i (x : xs) = let (r, rs) = getAndRemove (i - 1) xs in (r, x : rs)

moveLambdaArg :: GraphOp m => Port.OutPortId -> Int -> NodeRef -> m NodeRef
moveLambdaArg [] _ _ = throwM $ CannotRemovePortException
moveLambdaArg p@(Port.Projection port : []) newPosition lambda = match lambda $ \case
    Grouped g -> IR.source g >>= moveLambdaArg p newPosition >>= fmap IR.generalize . IR.grouped
    Lam _ _   -> do
        args <- ASTDeconstruct.extractArguments lambda
        out  <- ASTRead.getLambdaOutputRef      lambda
        let newArgs = shiftPosition port newPosition args
        lams newArgs out
    _ -> throwM $ NotLambdaException lambda

renameLambdaArg :: GraphOp m => Port.OutPortId -> String -> NodeRef -> m ()
renameLambdaArg [] _ _ = throwM CannotRemovePortException
renameLambdaArg p@(Port.Projection port : []) newName lam = match lam $ \case
    Grouped g -> IR.source g >>= renameLambdaArg p newName
    Lam _ _ -> do
        args <- ASTDeconstruct.extractArguments lam
        let arg = args !! port
        renameVar arg newName
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

replaceWhenBodyOrRef :: GraphOp m => NodeRef -> NodeRef -> m ()
replaceWhenBodyOrRef to from = do
    oldBody <- use $ Graph.breadcrumbHierarchy . BH.body
    when (oldBody == from) $ Graph.breadcrumbHierarchy . BH.body .= to
    oldRef  <- use $ Graph.breadcrumbHierarchy . BH.self
    when (oldRef == from)  $ Graph.breadcrumbHierarchy . BH.self .= to

replace :: GraphOp m => NodeRef -> NodeRef -> m ()
replace to from = do
    IR.replace to from
    replaceWhenBodyOrRef to from

substitute :: GraphOp m => NodeRef -> NodeRef -> m ()
substitute to from = do
    IR.substitute to from
    replaceWhenBodyOrRef to from
