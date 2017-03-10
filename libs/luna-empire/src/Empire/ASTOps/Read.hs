{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

{-|

This module consists only of operation that get information from
AST, without modifying it. They can still throw exceptions though.

-}

module Empire.ASTOps.Read where

import           Control.Monad                      ((>=>), forM)
import           Data.Coerce                        (coerce)
import           Data.Maybe                         (isJust)
import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp, match)
import           Empire.Data.AST                    (NodeRef, EdgeRef, NotUnifyException(..),
                                                     NotLambdaException(..),
                                                     astExceptionFromException, astExceptionToException)
import qualified Empire.Data.Graph                  as Graph
import qualified Empire.Data.BreadcrumbHierarchy    as BH
import           Empire.Data.Layers                 (NodeMarker(..), Marker)

import qualified OCI.IR.Combinators as IRExpr
import           Luna.IR.Term.Uni
import qualified Luna.IR as IR


isGraphNode :: ASTOp m => NodeRef -> m Bool
isGraphNode = fmap isJust . getNodeId

getNodeId :: ASTOp m => NodeRef -> m (Maybe NodeId)
getNodeId node = coerce <$> IR.readLayer @Marker node

getPatternNames :: ASTOp m => NodeRef -> m [String]
getPatternNames node = match node $ \case
    Var n     -> return [nameToString n]
    Cons _ as -> do
        args  <- mapM IR.source as
        names <- mapM getPatternNames args
        return $ concat names
    Blank{}   -> return ["_"]

getVarName :: ASTOp m => NodeRef -> m String
getVarName node = match node $ \case
    Var n    -> return $ nameToString n
    Cons n _ -> return $ pathNameToString n
    Blank{}  -> return "_"

rightMatchOperand :: ASTOp m => NodeRef -> m EdgeRef
rightMatchOperand node = match node $ \case
    Unify _ b -> pure b
    _         -> throwM $ NotUnifyException node

getTargetNode :: ASTOp m => NodeRef -> m NodeRef
getTargetNode node = rightMatchOperand node >>= IR.source

leftMatchOperand :: ASTOp m => NodeRef -> m EdgeRef
leftMatchOperand node = match node $ \case
    Unify a _ -> pure a
    _         -> throwM $ NotUnifyException node

getVarNode :: ASTOp m => NodeRef -> m NodeRef
getVarNode node = leftMatchOperand node >>= IR.source

data NodeDoesNotExistException = NodeDoesNotExistException NodeId
    deriving Show
instance Exception NodeDoesNotExistException where
    toException = astExceptionToException
    fromException = astExceptionFromException

getASTPointer :: ASTOp m => NodeId -> m NodeRef
getASTPointer nodeId = do
    -- TODO[MK]: checking if asking for own node is just to make specs pass, I need to figure out who and why needs it
    self <- use $ Graph.breadcrumbHierarchy . BH.self
    let selfRef = case self of
          Just (id, ref) -> if id == nodeId then Just $ BH.getAnyRef ref else Nothing
          Nothing -> Nothing
    case selfRef of
        Just r -> return r
        Nothing -> do
            node <- preuse (Graph.breadcrumbHierarchy . BH.children . ix nodeId . BH.self . _Just . _2)
            case node of
                Just target -> pure $ BH.getAnyRef target
                _           -> throwM $ NodeDoesNotExistException nodeId

getCurrentASTPointer :: ASTOp m => m (Maybe NodeRef)
getCurrentASTPointer = do
    node <- use $ Graph.breadcrumbHierarchy . BH.self
    case node of
        Just (_, tgt) -> return $ Just $ BH.getAnyRef tgt
        _             -> return $ Nothing

getASTTarget :: ASTOp m => NodeId -> m NodeRef
getASTTarget nodeId = do
    matchNode <- getASTPointer nodeId
    getTargetNode matchNode

getCurrentASTTarget :: ASTOp m => m (Maybe NodeRef)
getCurrentASTTarget = mapM getTargetNode =<< getCurrentASTPointer

getASTVar :: ASTOp m => NodeId -> m NodeRef
getASTVar nodeId = do
    matchNode <- getASTPointer nodeId
    getVarNode matchNode

getCurrentASTVar :: ASTOp m => m (Maybe NodeRef)
getCurrentASTVar = mapM getVarNode =<< getCurrentASTPointer

getSelfNodeRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getSelfNodeRef = getSelfNodeRef' False

getSelfNodeRef' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getSelfNodeRef' seenAcc node = match node $ \case
    Acc t _ -> IR.source t >>= getSelfNodeRef' True
    App t _ -> IR.source t >>= getSelfNodeRef' seenAcc
    _       -> return $ if seenAcc then Just node else Nothing

getLambdaBodyRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getLambdaBodyRef lam = do
    seqRef <- getLambdaSeqRef lam
    forM seqRef $ flip match $ \case
        Seq l _r -> IR.source l

getLambdaSeqRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getLambdaSeqRef = getLambdaSeqRef' False

getLambdaSeqRef' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getLambdaSeqRef' firstLam node = match node $ \case
    Lam _ next -> do
        nextLam <- IR.source next
        getLambdaSeqRef' True nextLam
    Seq{}     -> if firstLam then return $ Just node else throwM $ NotLambdaException node
    _         -> if firstLam then return Nothing     else throwM $ NotLambdaException node

getLambdaOutputRef :: ASTOp m => NodeRef -> m NodeRef
getLambdaOutputRef = getLambdaOutputLink >=> IR.source

getLambdaOutputLink :: ASTOp m => NodeRef -> m EdgeRef
getLambdaOutputLink = getLambdaOutputLink' False

getLambdaOutputLink' :: ASTOp m => Bool -> NodeRef -> m EdgeRef
getLambdaOutputLink' firstLam node = match node $ \case
    Lam _ next -> do
        nextLam <- IR.source next
        match nextLam $ \case
            Lam{} -> getLambdaOutputLink' True nextLam
            Seq{} -> getLambdaOutputLink' True nextLam
            _     -> return next
    Seq _l r  -> if firstLam then return r else throwM $ NotLambdaException node

getFirstNonLambdaRef :: ASTOp m => NodeRef -> m NodeRef
getFirstNonLambdaRef = getFirstNonLambdaRef' False

getFirstNonLambdaRef' :: ASTOp m => Bool -> NodeRef -> m NodeRef
getFirstNonLambdaRef' firstLam node = match node $ \case
    Lam _ next -> do
        nextLam <- IR.source next
        getFirstNonLambdaRef' True nextLam
    _         -> if firstLam then return node else throwM $ NotLambdaException node

isApp :: ASTOp m => NodeRef -> m Bool
isApp expr = isJust <$> IRExpr.narrowTerm @IR.App expr

isBlank :: ASTOp m => NodeRef -> m Bool
isBlank expr = isJust <$> IRExpr.narrowTerm @IR.Blank expr

isLambda :: ASTOp m => NodeRef -> m Bool
isLambda expr = isJust <$> IRExpr.narrowTerm @IR.Lam expr

isMatch :: ASTOp m => NodeRef -> m Bool
isMatch expr = isJust <$> IRExpr.narrowTerm @IR.Unify expr

rhsIsLambda :: ASTOp m => NodeRef -> m Bool
rhsIsLambda ref = do
    rhs <- getTargetNode ref
    isLambda rhs

canEnterNode :: ASTOp m => NodeRef -> m Bool
canEnterNode ref = do
    match' <- isMatch ref
    if match' then rhsIsLambda ref else return False
