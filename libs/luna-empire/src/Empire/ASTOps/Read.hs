{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|

This module consists only of operation that get information from
AST, without modifying it. They can still throw exceptions though.

-}

module Empire.ASTOps.Read (
    isGraphNode
  , getNodeId
  , getVarName
  , isApp
  , isBlank
  , isLambda
  , isMatch
  , getASTPointer
  , getASTTarget
  , getASTVar
  , getTargetNode
  , getVarNode
  , getLambdaBodyRef
  , getFirstNonLambdaRef
  , getLambdaOutputLink
  , getLambdaOutputRef
  , getLambdaSeqRef
  , getPatternNames
  , getSelfNodeRef
  , canEnterNode
  , rhsIsLambda
  , varIsPatternMatch
  , nodeIsPatternMatch
  ) where

import           Control.Monad                      ((>=>), forM)
import           Control.Monad.Catch                (Handler(..), catches)
import           Data.Coerce                        (coerce)
import           Data.Maybe                         (isJust)
import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp, match)
import           Empire.Data.AST                    (NodeRef, EdgeRef, NotUnifyException(..),
                                                     NotLambdaException(..),
                                                     astExceptionFromException, astExceptionToException)
import qualified Empire.Data.Graph                  as Graph
import           Empire.Data.Layers                 (NodeMarker(..), Marker)

import qualified Luna.IR.Expr.Combinators as IRExpr
import           Luna.IR.Expr.Term.Uni
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
    Cons n _ -> return $ nameToString n
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
    node <- use (Graph.nodeMapping . at nodeId)
    case node of
        Just target -> pure $ Graph.getAnyRef target
        _           -> throwM $ NodeDoesNotExistException nodeId

getASTTarget :: ASTOp m => NodeId -> m NodeRef
getASTTarget nodeId = do
    matchNode <- getASTPointer nodeId
    getTargetNode matchNode

getASTVar :: ASTOp m => NodeId -> m NodeRef
getASTVar nodeId = do
    matchNode <- getASTPointer nodeId
    getVarNode matchNode

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
isApp expr = isJust <$> IRExpr.narrowAtom @IR.App expr

isBlank :: ASTOp m => NodeRef -> m Bool
isBlank expr = isJust <$> IRExpr.narrowAtom @IR.Blank expr

isLambda :: ASTOp m => NodeRef -> m Bool
isLambda expr = isJust <$> IRExpr.narrowAtom @IR.Lam expr

isMatch :: ASTOp m => NodeRef -> m Bool
isMatch expr = isJust <$> IRExpr.narrowAtom @IR.Unify expr

isCons :: ASTOp m => NodeRef -> m Bool
isCons expr = isJust <$> IRExpr.narrowAtom @IR.Cons expr

nodeIsPatternMatch :: ASTOp m => NodeId -> m Bool
nodeIsPatternMatch nid = (do
    root <- getASTPointer nid
    varIsPatternMatch root) `catches` [
          Handler (\(e :: NotUnifyException)         -> return False)
        , Handler (\(e :: NodeDoesNotExistException) -> return False)
        ]

varIsPatternMatch :: ASTOp m => NodeRef -> m Bool
varIsPatternMatch expr = do
    var <- getVarNode expr
    isCons var

rhsIsLambda :: ASTOp m => NodeId -> m Bool
rhsIsLambda nid = do
    node <- getASTTarget nid
    isLambda node

canEnterNode :: ASTOp m => NodeId -> m Bool
canEnterNode nid = do
    root   <- getASTPointer nid
    match' <- isMatch root
    if match' then rhsIsLambda nid else return False
