module Empire.Commands.GraphUtils where

import           Prologue

import           Empire.Empire

import           Empire.Data.AST         (NodeRef)
import           Empire.Data.Graph       (Graph)
import qualified Empire.Data.Graph       as Graph
import           Empire.API.Data.Node    (NodeId)
import qualified Empire.Commands.AST     as AST



getASTPointer :: NodeId -> Command Graph NodeRef
getASTPointer nodeId = Graph.getAnyRef <$> (use (Graph.nodeMapping . at nodeId) <?!> err)
    where
        err = "Node " ++ show nodeId ++ " does not exist"

getASTTarget :: NodeId -> Command Graph NodeRef
getASTTarget nodeId = do
    matchNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.getTargetNode matchNode

getASTVar :: NodeId -> Command Graph NodeRef
getASTVar nodeId = do
    matchNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.getVarNode matchNode

rewireNode :: NodeId -> NodeRef -> Command Graph ()
rewireNode nodeId newTarget = do
    matchNode <- getASTPointer nodeId
    oldTarget <- getASTTarget  nodeId
    zoom Graph.ast $ do
        AST.replaceTargetNode matchNode newTarget
        AST.removeSubtree oldTarget
