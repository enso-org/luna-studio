module Luna.Studio.Action.Batch  where

import           Data.UUID.Types                      (UUID)
import           Empire.API.Data.Connection           (Connection, ConnectionId)
import           Empire.API.Data.Node                 (Node, NodeId)
import           Empire.API.Data.NodeMeta             (NodeMeta)
import           Empire.API.Data.PortDefault          (PortDefault)
import           Empire.API.Data.PortRef              (AnyPortRef, InPortRef, OutPortRef, dstNodeId, nodeId)
import           Empire.API.Data.Project              (ProjectId)
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Action.UUID              (registerRequest)
import qualified Luna.Studio.Batch.Connector.Commands as BatchCmd
import           Luna.Studio.Batch.Workspace          (Workspace)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global             (State, clientId, workspace)


withWorkspace :: (Workspace -> UUID -> Maybe UUID -> IO ()) -> Command State ()
withWorkspace act = do
    uuid       <- registerRequest
    guiID      <- use clientId
    workspace' <- use workspace
    liftIO $ act workspace' uuid $ Just guiID

withWorkspace' :: (Workspace -> IO ()) -> Command State ()
withWorkspace' act = do
    workspace' <- use workspace
    liftIO $ act workspace'

withUUID :: (UUID -> Maybe UUID -> IO ()) -> Command State ()
withUUID act = do
    uuid  <- registerRequest
    guiID <- use clientId
    liftIO $ act uuid $ Just guiID

createLibrary :: Text -> Text -> Command State ()
createLibrary = withWorkspace .: BatchCmd.createLibrary

listLibraries :: ProjectId -> Command State ()
listLibraries = withUUID . BatchCmd.listLibraries


createProject :: Text -> Command State ()
createProject = withUUID . BatchCmd.createProject

exportProject :: ProjectId -> Command State ()
exportProject = withUUID . BatchCmd.exportProject

importProject :: Text -> Command State ()
importProject = withUUID . BatchCmd.importProject

listProjects :: Command State ()
listProjects = withUUID BatchCmd.listProjects

openProject :: FilePath -> Command State ()
openProject = withUUID . BatchCmd.openProject


dumpGraphViz :: Command State ()
dumpGraphViz = withWorkspace BatchCmd.dumpGraphViz


getProgram :: Command State ()
getProgram = withWorkspace BatchCmd.getProgram


addConnection :: Either OutPortRef NodeId -> Either InPortRef NodeId -> Command State ()
addConnection src dst = do
    collaborativeModify . return $ either (view dstNodeId) id dst
    withWorkspace $ BatchCmd.addConnection src dst

addNode :: NodeId -> Text -> NodeMeta -> Maybe NodeId -> Command State ()
addNode = withWorkspace .:: BatchCmd.addNode

addPort :: AnyPortRef -> Command State ()
addPort = withWorkspace . BatchCmd.addPort

addSubgraph :: [Node] -> [Connection] -> Command State ()
addSubgraph = withWorkspace .: BatchCmd.addSubgraph

movePort :: AnyPortRef -> AnyPortRef -> Command State ()
movePort = withWorkspace .: BatchCmd.movePort

redo :: Command State ()
redo = withUUID BatchCmd.redo

removeConnection :: ConnectionId -> Command State ()
removeConnection connId = do
    collaborativeModify [connId ^. dstNodeId]
    withWorkspace $ BatchCmd.removeConnection connId

removeNodes :: [NodeId] -> Command State ()
removeNodes = withWorkspace . BatchCmd.removeNodes

removePort :: AnyPortRef -> Command State ()
removePort = withWorkspace . BatchCmd.removePort

renameNode :: NodeId -> Text -> Command State ()
renameNode = withWorkspace .:  BatchCmd.renameNode

renamePort :: AnyPortRef -> String -> Command State ()
renamePort = withWorkspace .: BatchCmd.renamePort

searchNodes :: Text -> (Int, Int) -> Command State ()
searchNodes = withWorkspace .: BatchCmd.searchNodes

-- TODO[LJK, PM]: Probably remove
-- setInputNodeType :: NodeId -> Text -> Command State ()
-- setInputNodeType = withWorkspace .: BatchCmd.setInputNodeType

setNodeCode :: NodeId -> Text -> Command State ()
setNodeCode = withWorkspace .:  BatchCmd.setNodeCode

setNodeExpression :: NodeId -> Text -> Command State ()
setNodeExpression = withWorkspace .: BatchCmd.setNodeExpression

setNodesMeta :: [(NodeId, NodeMeta)] -> Command State ()
setNodesMeta = withWorkspace . BatchCmd.setNodesMeta

setPortDefault :: AnyPortRef -> PortDefault -> Command State ()
setPortDefault portRef portDefault = do
    collaborativeModify [portRef ^. nodeId]
    withWorkspace $ BatchCmd.setPortDefault portRef portDefault

undo :: Command State ()
undo = withUUID BatchCmd.undo


requestCollaborationRefresh :: Command State ()
requestCollaborationRefresh = do
    clId <- use clientId
    withWorkspace' $ BatchCmd.requestCollaborationRefresh clId

collaborativeTouch :: [NodeId] -> Command State ()
collaborativeTouch nodeIds = unless (null nodeIds) $ do
    clId <- use clientId
    withWorkspace' $ BatchCmd.collaborativeTouch clId nodeIds

collaborativeModify :: [NodeId] -> Command State ()
collaborativeModify nodeIds = unless (null nodeIds) $ do
    clId <- use clientId
    withWorkspace' $ BatchCmd.collaborativeModify clId nodeIds

cancelCollaborativeTouch :: [NodeId] -> Command State ()
cancelCollaborativeTouch nodeIds = unless (null nodeIds) $ do
    clId <- use clientId
    withWorkspace' $ BatchCmd.cancelCollaborativeTouch clId nodeIds
