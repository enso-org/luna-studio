module Luna.Studio.Action.Batch  where

import           Data.Position                        (Position, toTuple)
import           Data.UUID.Types                      (UUID)
import           Empire.API.Data.Connection           (Connection (Connection))
import           Empire.API.Data.NodeMeta             (NodeMeta (NodeMeta))
import           Empire.API.Data.PortDefault          (PortDefault)
import           Empire.API.Data.PortRef              (AnyPortRef (InPortRef', OutPortRef'), InPortRef (InPortRef), OutPortRef (OutPortRef),
                                                       dstNodeId, nodeId)
import           Empire.API.Data.Project              (ProjectId)
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Action.UUID              (registerRequest)
import qualified Luna.Studio.Batch.Connector.Commands as BatchCmd
import           Luna.Studio.Batch.Workspace          (Workspace)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection   (ConnectionId)
import           Luna.Studio.React.Model.Node         (ExpressionNode, NodeId)
import           Luna.Studio.State.Global             (State, backend, clientId, workspace)


withWorkspace :: (Workspace -> UUID -> Maybe UUID -> IO ()) -> Command State ()
withWorkspace act = do
    uuid       <- registerRequest
    guiID      <- use $ backend . clientId
    workspace' <- use workspace
    liftIO $ act workspace' uuid $ Just guiID

withWorkspace' :: (Workspace -> IO ()) -> Command State ()
withWorkspace' act = do
    workspace' <- use workspace
    liftIO $ act workspace'

withUUID :: (UUID -> Maybe UUID -> IO ()) -> Command State ()
withUUID act = do
    uuid  <- registerRequest
    guiID <- use $ backend . clientId
    liftIO $ act uuid $ Just guiID

-- createLibrary :: Text -> Text -> Command State ()
-- createLibrary = withWorkspace .: BatchCmd.createLibrary
--
-- listLibraries :: ProjectId -> Command State ()
-- listLibraries = withUUID . BatchCmd.listLibraries
--
--
-- createProject :: Text -> Command State ()
-- createProject = withUUID . BatchCmd.createProject
--
-- exportProject :: ProjectId -> Command State ()
-- exportProject = withUUID . BatchCmd.exportProject
--
-- importProject :: Text -> Command State ()
-- importProject = withUUID . BatchCmd.importProject
--
-- listProjects :: Command State ()
-- listProjects = withUUID BatchCmd.listProjects
--
-- openProject :: FilePath -> Command State ()
-- openProject = withUUID . BatchCmd.openProject


dumpGraphViz :: Command State ()
dumpGraphViz = withWorkspace BatchCmd.dumpGraphViz


-- getProgram :: Command State ()
-- getProgram = withWorkspace BatchCmd.getProgram


addConnection :: Either OutPortRef NodeId -> Either AnyPortRef NodeId -> Command State ()
addConnection src dst = do
    let nid = case dst of
            Left (OutPortRef' (OutPortRef nid' _)) -> nid'
            Left (InPortRef'  (InPortRef  nid' _)) -> nid'
            Right nid'                             -> nid'
    collaborativeModify [nid]
    withWorkspace $ BatchCmd.addConnection src dst

addNode :: NodeId -> Text -> Position -> Bool -> Maybe NodeId -> Command State ()
addNode nid expr pos dispRes connectTo = withWorkspace $ BatchCmd.addNode nid expr (NodeMeta (toTuple pos) dispRes) connectTo

addPort :: AnyPortRef -> Command State ()
addPort = withWorkspace . BatchCmd.addPort

addSubgraph :: [ExpressionNode] -> [(OutPortRef, InPortRef)] -> Command State ()
addSubgraph nodes conns = withWorkspace $ BatchCmd.addSubgraph (map convert nodes) (map (uncurry Connection) conns)

getSubgraph :: NodeId -> Command State ()
getSubgraph nid = withWorkspace (BatchCmd.getSubgraph nid)

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

setNodesMeta :: [(NodeId, Position, Bool)] -> Command State ()
setNodesMeta = withWorkspace . BatchCmd.setNodesMeta . map convert

setPortDefault :: AnyPortRef -> PortDefault -> Command State ()
setPortDefault portRef portDefault = do
    collaborativeModify [portRef ^. nodeId]
    withWorkspace $ BatchCmd.setPortDefault portRef portDefault

undo :: Command State ()
undo = withUUID BatchCmd.undo


requestCollaborationRefresh :: Command State ()
requestCollaborationRefresh = do
    clId <- use $ backend . clientId
    withWorkspace' $ BatchCmd.requestCollaborationRefresh clId

collaborativeTouch :: [NodeId] -> Command State ()
collaborativeTouch nodeIds = unless (null nodeIds) $ do
    clId <- use $ backend . clientId
    withWorkspace' $ BatchCmd.collaborativeTouch clId nodeIds

collaborativeModify :: [NodeId] -> Command State ()
collaborativeModify nodeIds = unless (null nodeIds) $ do
    clId <- use $ backend . clientId
    withWorkspace' $ BatchCmd.collaborativeModify clId nodeIds

cancelCollaborativeTouch :: [NodeId] -> Command State ()
cancelCollaborativeTouch nodeIds = unless (null nodeIds) $ do
    clId <- use $ backend . clientId
    withWorkspace' $ BatchCmd.cancelCollaborativeTouch clId nodeIds
