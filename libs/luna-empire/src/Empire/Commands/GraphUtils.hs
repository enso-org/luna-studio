module Empire.Commands.GraphUtils (
    getASTPointer
  , getASTTarget
  , getASTVar

  , rewireNode
  ) where

import           Empire.Prelude ()

import           Empire.ASTOp            (ASTOp)
import           Empire.ASTOps.Read      (getASTPointer, getASTTarget, getASTVar)
import           Empire.ASTOps.Remove    (removeSubtree)
import           Empire.Data.AST         (NodeRef)
import           Empire.API.Data.Node    (NodeId)
import qualified Empire.Commands.AST     as AST



rewireNode :: ASTOp m => NodeId -> NodeRef -> m ()
rewireNode nodeId newTarget = do
    matchNode <- getASTPointer nodeId
    oldTarget <- getASTTarget  nodeId
    AST.replaceTargetNode matchNode newTarget
    removeSubtree oldTarget
