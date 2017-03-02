module Luna.Studio.Action.Batch  where

import           Data.UUID.Types                      (UUID)
import           Empire.API.Data.Connection           (Connection)
import qualified Empire.API.Data.DefaultValue         as DefaultValue
import           Empire.API.Data.Node                 (Node, NodeId)
import           Empire.API.Data.NodeMeta             (NodeMeta)
import           Empire.API.Data.PortRef              (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef              as PortRef (dstNodeId, nodeId)
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
    guiID      <- use $ clientId
    workspace' <- use workspace
    liftIO $ act workspace' uuid $ Just guiID

withWorkspace' :: (Workspace -> IO ()) -> Command State ()
withWorkspace' act = do
    workspace' <- use workspace
    liftIO $ act workspace'

withUUID :: (UUID -> Maybe UUID -> IO ()) -> Command State ()
withUUID act = do
    uuid  <- registerRequest
    guiID <- use $ clientId
    liftIO $ act uuid $ Just guiID

addNode :: Text -> NodeMeta -> Maybe NodeId -> Command State ()
addNode = withWorkspace .:. BatchCmd.addNode

addPort :: NodeId -> Command State ()
addPort = withWorkspace . BatchCmd.addPort

addSubgraph :: [Node] -> [Connection] -> Command State ()
addSubgraph = withWorkspace .: BatchCmd.addSubgraph

createProject :: Text -> Command State ()
createProject = withUUID . BatchCmd.createProject

listProjects :: Command State ()
listProjects = withUUID BatchCmd.listProjects

createLibrary :: Text -> Text -> Command State ()
createLibrary = withWorkspace .: BatchCmd.createLibrary

listLibraries :: ProjectId -> Command State ()
listLibraries = withUUID . BatchCmd.listLibraries

getProgram :: Command State ()
getProgram = withWorkspace BatchCmd.getProgram

updateNodeExpression :: NodeId -> Text -> Command State ()
updateNodeExpression = withWorkspace .: BatchCmd.updateNodeExpression

updateNodeMeta :: [(NodeId, NodeMeta)] -> Command State ()
updateNodeMeta = withWorkspace . BatchCmd.updateNodeMeta

renameNode :: NodeId -> Text -> Command State ()
renameNode = withWorkspace .:  BatchCmd.renameNode

setCode :: NodeId -> Text -> Command State ()
setCode = withWorkspace .:  BatchCmd.setCode

removeNodes :: [NodeId] -> Command State ()
removeNodes = withWorkspace . BatchCmd.removeNodes

renamePort :: AnyPortRef -> String -> Command State ()
renamePort = withWorkspace .: BatchCmd.renamePort

movePort :: AnyPortRef -> Int -> Command State ()
movePort = withWorkspace .: BatchCmd.movePort

removePort :: AnyPortRef -> Command State ()
removePort = withWorkspace . BatchCmd.removePort

connect :: Either OutPortRef NodeId -> Either InPortRef NodeId -> Command State ()
connect src dst = do
    let nodeId = case dst of
            Left portRef  -> portRef ^. PortRef.dstNodeId
            Right nodeId' -> nodeId'
    collaborativeModify [nodeId]
    withWorkspace $ BatchCmd.connect src dst where

disconnect :: InPortRef -> Command State ()
disconnect dst = do
    collaborativeModify [dst ^. PortRef.dstNodeId]
    withWorkspace $ BatchCmd.disconnect dst

setDefaultValue :: AnyPortRef -> DefaultValue.PortDefault -> Command State ()
setDefaultValue portRef value = do
    collaborativeModify [portRef ^. PortRef.nodeId]
    withWorkspace $ BatchCmd.setDefaultValue portRef value

setInputNodeType :: NodeId -> Text -> Command State ()
setInputNodeType = withWorkspace .: BatchCmd.setInputNodeType

nodeSearch :: Text -> (Int, Int) -> Command State ()
nodeSearch = withWorkspace .: BatchCmd.nodeSearch

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

exportProject :: ProjectId -> Command State ()
exportProject = withUUID . BatchCmd.exportProject

importProject :: Text -> Command State ()
importProject = withUUID . BatchCmd.importProject

openProject :: FilePath -> Command State ()
openProject = withUUID . BatchCmd.openProject

dumpGraphViz :: Command State ()
dumpGraphViz = withWorkspace BatchCmd.dumpGraphViz

requestRedo :: Command State ()
requestRedo = withUUID BatchCmd.requestRedo

requestUndo :: Command State ()
requestUndo = withUUID BatchCmd.requestUndo
