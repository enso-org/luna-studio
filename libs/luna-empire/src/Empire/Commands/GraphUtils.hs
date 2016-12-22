module Empire.Commands.GraphUtils where

import           Empire.Prelude

import           Empire.Empire

import           Data.Maybe              (fromJust)
import           Empire.ASTOp            (ASTOp)
import           Empire.Data.AST         (NodeRef)
import           Empire.Data.Graph       (Graph)
import qualified Empire.Data.Graph       as Graph
import           Empire.API.Data.Node    (NodeId)
import qualified Empire.Commands.AST     as AST



data NodeDoesNotExistException = NodeDoesNotExistException NodeId
    deriving Show
instance Exception NodeDoesNotExistException

getASTPointer :: ASTOp m => NodeId -> m NodeRef
getASTPointer nodeId = do
    node <- use (Graph.nodeMapping . at nodeId)
    case node of
        Just target -> pure $ Graph.getAnyRef target
        _           -> throwM $ NodeDoesNotExistException nodeId

getASTTarget :: ASTOp m => NodeId -> m NodeRef
getASTTarget nodeId = do
    matchNode <- getASTPointer nodeId
    AST.getTargetNode matchNode

getASTVar :: ASTOp m => NodeId -> m NodeRef
getASTVar nodeId = do
    matchNode <- getASTPointer nodeId
    AST.getVarNode matchNode

rewireNode :: ASTOp m => NodeId -> NodeRef -> m ()
rewireNode nodeId newTarget = do
    matchNode <- getASTPointer nodeId
    oldTarget <- getASTTarget  nodeId
    AST.replaceTargetNode matchNode newTarget
    AST.removeSubtree oldTarget
