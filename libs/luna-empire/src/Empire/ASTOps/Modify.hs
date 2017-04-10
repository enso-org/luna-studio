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

import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Port               as Port
import           Empire.ASTOp                       (ASTOp, match)
import qualified Empire.ASTOps.Builder              as ASTBuilder
import qualified Empire.ASTOps.Deconstruct          as ASTDeconstruct
import qualified Empire.ASTOps.Read                 as ASTRead
import qualified Empire.ASTOps.Remove               as ASTRemove
import           Empire.Data.AST                    (EdgeRef, NodeRef, NotLambdaException(..),
                                                     NotUnifyException(..), astExceptionToException,
                                                     astExceptionFromException)

import qualified OCI.IR.Combinators as IR (replaceSource, narrowTerm, replace, substitute)
import           Luna.IR.Term.Uni
import qualified Luna.IR as IR



addLambdaArg :: ASTOp m => Int -> NodeRef -> m ()
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

getArgNames :: ASTOp m => NodeRef -> m [String]
getArgNames ref = match ref $ \case
    Grouped g   -> IR.source g >>= getArgNames
    Lam a body -> do
        argNames <- ASTRead.getPatternNames =<< IR.source a
        (argNames ++) <$> (getArgNames =<< IR.source body)
    _ -> return []

replaceWithLam :: ASTOp m => Maybe EdgeRef -> String -> NodeRef -> m ()
replaceWithLam parent name lam = do
    tmpBlank <- IR.blank
    binder   <- IR.var $ stringToName name
    newLam   <- IR.lam binder tmpBlank
    case parent of
        Just e  -> IR.replaceSource (IR.generalize newLam) e
        Nothing -> IR.substitute newLam lam
    IR.replace lam tmpBlank
    return ()

addLambdaArg' :: ASTOp m => Int -> String -> Maybe EdgeRef -> NodeRef -> m ()
addLambdaArg' 0   name parent lam = replaceWithLam parent name lam
addLambdaArg' pos name parent lam = match lam $ \case
    Lam _ b -> addLambdaArg' (pos - 1) name (Just b) =<< IR.source b
    _       -> replaceWithLam parent name lam

data CannotRemovePortException = CannotRemovePortException
    deriving Show

instance Exception CannotRemovePortException where
    toException = astExceptionToException
    fromException = astExceptionFromException

removeLambdaArg :: ASTOp m => Port.PortId -> NodeRef -> m NodeRef
removeLambdaArg Port.InPortId{}     _ = throwM $ CannotRemovePortException
removeLambdaArg (Port.OutPortId []) _ = throwM $ CannotRemovePortException
removeLambdaArg p@(Port.OutPortId (Port.Projection port : [])) lambda = match lambda $ \case
    Grouped g      -> IR.source g >>= removeLambdaArg p >>= fmap IR.generalize . IR.grouped
    Lam _arg _body -> do
        args <- ASTDeconstruct.extractArguments lambda
        out  <- ASTRead.getFirstNonLambdaRef lambda
        let newArgs = args ^.. folded . ifiltered (\i _ -> i /= port)
        ASTBuilder.lams newArgs out
    _ -> throwM $ NotLambdaException lambda

shiftPosition :: Int -> Int -> [a] -> [a]
shiftPosition from to lst = uncurry (insertAt to) $ getAndRemove from lst where
    insertAt 0 e l        = e : l
    insertAt i e (x : xs) = x : insertAt (i - 1) e xs

    getAndRemove 0 (x : xs) = (x, xs)
    getAndRemove i (x : xs) = let (r, rs) = getAndRemove (i - 1) xs in (r, x : rs)

moveLambdaArg :: ASTOp m => Port.PortId -> Int -> NodeRef -> m NodeRef
moveLambdaArg Port.InPortId{}     _ _ = throwM $ CannotRemovePortException
moveLambdaArg (Port.OutPortId []) _ _ = throwM $ CannotRemovePortException
moveLambdaArg p@(Port.OutPortId (Port.Projection port : [])) newPosition lambda = match lambda $ \case
    Grouped g -> IR.source g >>= moveLambdaArg p newPosition >>= fmap IR.generalize . IR.grouped
    Lam _ _   -> do
        args <- ASTDeconstruct.extractArguments lambda
        out  <- ASTRead.getLambdaOutputRef      lambda
        let newArgs = shiftPosition port newPosition args
        ASTBuilder.lams newArgs out
    _ -> throwM $ NotLambdaException lambda

renameLambdaArg :: ASTOp m => Port.PortId -> String -> NodeRef -> m ()
renameLambdaArg Port.InPortId{}     _ _ = throwM CannotRemovePortException
renameLambdaArg (Port.OutPortId []) _ _ = throwM CannotRemovePortException
renameLambdaArg p@(Port.OutPortId (Port.Projection port : [])) newName lam = match lam $ \case
    Grouped g -> IR.source g >>= renameLambdaArg p newName
    Lam _ _ -> do
        args <- ASTDeconstruct.extractArguments lam
        let arg = args !! port
        renameVar arg newName
    _ -> throwM $ NotLambdaException lam

redirectLambdaOutput :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
redirectLambdaOutput lambda newOutputRef = do
    match lambda $ \case
        Grouped g   -> IR.source g >>= flip redirectLambdaOutput newOutputRef >>= fmap IR.generalize . IR.grouped
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            ASTBuilder.lams args' newOutputRef
        _ -> throwM $ NotLambdaException lambda

setLambdaOutputToBlank :: ASTOp m => NodeRef -> m NodeRef
setLambdaOutputToBlank lambda = do
    match lambda $ \case
        Grouped g   -> IR.source g >>= setLambdaOutputToBlank >>= fmap IR.generalize . IR.grouped
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            blank <- IR.generalize <$> IR.blank
            ASTBuilder.lams args' blank
        _ -> throwM $ NotLambdaException lambda

replaceTargetNode :: ASTOp m => NodeRef -> NodeRef -> m ()
replaceTargetNode matchNode newTarget = do
    match matchNode $ \case
        Unify _l r -> do
            IR.replaceSource newTarget r
        _ -> throwM $ NotUnifyException matchNode

replaceVarNode :: ASTOp m => NodeRef -> NodeRef -> m ()
replaceVarNode matchNode newVar = do
    match matchNode $ \case
        Unify l _r -> do
            IR.replaceSource newVar l
        _ -> throwM $ NotUnifyException matchNode

rewireNode :: ASTOp m => NodeId -> NodeRef -> m ()
rewireNode nodeId newTarget = do
    matchNode <- ASTRead.getASTPointer nodeId
    oldTarget <- ASTRead.getASTTarget  nodeId
    replaceTargetNode matchNode newTarget
    ASTRemove.removeSubtree oldTarget

rewireNodeName :: ASTOp m => NodeId -> NodeRef -> m ()
rewireNodeName nodeId newVar = do
    matchNode <- ASTRead.getASTPointer nodeId
    oldVar    <- ASTRead.getASTVar  nodeId
    replaceVarNode matchNode newVar
    ASTRemove.removeSubtree oldVar

rewireCurrentNode :: ASTOp m => NodeRef -> m ()
rewireCurrentNode newTarget = do
    Just matchNode <- ASTRead.getCurrentASTPointer
    Just oldTarget <- ASTRead.getCurrentASTTarget
    replaceTargetNode matchNode newTarget
    ASTRemove.removeSubtree oldTarget

renameVar :: ASTOp m => NodeRef -> String -> m ()
renameVar vref name = do
    var <- IR.narrowTerm @IR.Var vref
    mapM_ (flip IR.modifyExprTerm $ IR.name .~ (stringToName name)) var
