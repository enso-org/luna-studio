module Empire.Commands.Graph.NodeMeta where

import Empire.Prelude

import qualified Empire.ASTOps.Read       as ASTRead
import qualified Empire.Commands.AST      as AST
import qualified LunaStudio.Data.NodeMeta as NodeMeta

import Empire.ASTOp                  (ClassOp, GraphOp, runASTOp)
import Empire.Commands.Graph.Context (withBreadcrumb, withGraph)
import Empire.Data.Graph             (ClsGraph)
import Empire.Empire                 (Command, Empire)
import LunaStudio.Data.GraphLocation (GraphLocation)
import LunaStudio.Data.NodeId        (NodeId)
import LunaStudio.Data.NodeMeta      (NodeMeta)
import LunaStudio.Data.Position      (Position)


getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc = withGraph loc . runASTOp . AST.getNodeMeta

setNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
setNodeMeta gl nodeId newMeta = withBreadcrumb
    gl
    (runASTOp $ setNodeMetaGraph nodeId newMeta)
    (setNodeMetaFun nodeId newMeta)

setNodePosition :: GraphLocation -> NodeId -> Position -> Empire ()
setNodePosition loc nodeId newPos = do
    oldMeta <- fromMaybe def <$> getNodeMeta loc nodeId
    void .setNodeMeta loc nodeId $ oldMeta & NodeMeta.position .~ newPos

setNodeMetaGraph :: NodeId -> NodeMeta -> GraphOp ()
setNodeMetaGraph nodeId newMeta = ASTRead.getASTRef nodeId >>= \ref ->
    void $ AST.writeMeta ref newMeta

setNodeMetaFun :: NodeId -> NodeMeta -> Command ClsGraph ()
setNodeMetaFun nodeId newMeta = runASTOp $ do
    f <- ASTRead.getFunByNodeId nodeId
    void $ AST.writeMeta f newMeta

setNodePositionAST :: NodeId -> Position -> GraphOp ()
setNodePositionAST nodeId newPos = do
    ref     <- ASTRead.getASTRef nodeId
    oldMeta <- fromMaybe def <$> AST.readMeta ref
    AST.writeMeta ref $ oldMeta & NodeMeta.position .~ newPos

setNodePositionCls :: NodeId -> Position -> ClassOp ()
setNodePositionCls nodeId newPos = do
    f       <- ASTRead.getFunByNodeId nodeId
    oldMeta <- fromMaybe def <$> AST.readMeta f
    AST.writeMeta f $ oldMeta & NodeMeta.position .~ newPos
