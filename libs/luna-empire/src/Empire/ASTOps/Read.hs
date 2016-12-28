{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

{-|

This module consists only of operation that get information from
AST, without modifying it. They can still throw exceptions though.

-}

module Empire.ASTOps.Read (
    isGraphNode
  , getNodeId
  , getVarName
  , getName
  , isApp
  , isBlank
  , isLambda
  , isMatch
  , getASTPointer
  , getASTTarget
  , getASTVar
  ) where

import           Data.Coerce                        (coerce)
import           Data.Maybe                         (isJust)
import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp)
import           Empire.Data.AST                    (NodeRef, EdgeRef, NotUnifyException(..),
                                                     astExceptionFromException, astExceptionToException)
import qualified Empire.Data.Graph                  as Graph
import           Empire.Data.Layers                 (NodeMarker(..), Marker)

import qualified Luna.IR.Expr.Combinators as IRExpr
import Luna.IR.Expr.Term.Uni
import Luna.IR.Function (arg)
import           Luna.IR.Function.Argument (Arg)
import qualified Luna.IR.Function.Argument as Arg
import           Luna.IR (match)
import qualified Luna.IR as IR


isGraphNode :: ASTOp m => NodeRef -> m Bool
isGraphNode = fmap isJust . getNodeId

getNodeId :: ASTOp m => NodeRef -> m (Maybe NodeId)
getNodeId node = coerce <$> IR.readLayer @Marker node

getVarName :: ASTOp m => NodeRef -> m String
getVarName node = match node $ \case
    Var n -> getName n

getName :: ASTOp m => EdgeRef -> m String
getName node = do
    str <- IR.source node
    match str $ \case
        IR.String s -> return s

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

isApp :: ASTOp m => NodeRef -> m Bool
isApp expr = isJust <$> IRExpr.narrowAtom @IR.App expr

isBlank :: ASTOp m => NodeRef -> m Bool
isBlank expr = isJust <$> IRExpr.narrowAtom @IR.Blank expr

isLambda :: ASTOp m => NodeRef -> m Bool
isLambda expr = isJust <$> IRExpr.narrowAtom @IR.Lam expr

isMatch :: ASTOp m => NodeRef -> m Bool
isMatch expr = isJust <$> IRExpr.narrowAtom @IR.Unify expr
