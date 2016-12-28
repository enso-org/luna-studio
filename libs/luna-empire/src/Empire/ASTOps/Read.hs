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
  ) where

import           Data.Coerce                        (coerce)
import           Data.Maybe                         (isJust)
import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp)
import           Empire.Data.AST                    (NodeRef, EdgeRef)
import           Empire.Data.Layers                 (NodeMarker(..), Marker)

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
