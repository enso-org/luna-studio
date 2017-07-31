module NodeEditor.Action.Batch  where

import           Common.Action.Command               (Command)
import           Common.Prelude
import           Data.UUID.Types                     (UUID)
import           LunaStudio.Data.GraphLocation       (GraphLocation)
import           LunaStudio.Data.NodeMeta            (NodeMeta)
import           LunaStudio.Data.PortDefault         (PortDefault)
import           LunaStudio.Data.PortRef             (AnyPortRef (InPortRef', OutPortRef'), InPortRef (InPortRef), OutPortRef (OutPortRef),
                                                      dstNodeLoc, nodeLoc)
import           LunaStudio.Data.Position            (Position)
import           LunaStudio.Data.Project             (LocationSettings)
import           NodeEditor.Action.Command           (Command)
import           NodeEditor.Action.UUID              (registerRequest)
import qualified NodeEditor.Batch.Connector.Commands as BatchCmd
import           NodeEditor.Batch.Workspace          (Workspace)
import           NodeEditor.React.Model.Connection   (ConnectionId)
import           NodeEditor.React.Model.Node         (ExpressionNode, NodeLoc)
import           NodeEditor.State.Global             (State, backend, clientId, workspace)


withWorkspace :: (Workspace -> UUID -> Maybe UUID -> IO ()) -> Command State ()
withWorkspace act = do
    uuid       <- registerRequest
    guiID      <- use $ backend . clientId
    withJustM (use workspace) $ \workspace' ->
        liftIO $ act workspace' uuid $ Just guiID

withWorkspace' :: (Workspace -> IO ()) -> Command State ()
withWorkspace' act = do
    withJustM (use workspace) $ liftIO . act

withUUID :: (UUID -> Maybe UUID -> IO ()) -> Command State ()
withUUID act = do
    uuid  <- registerRequest
    guiID <- use $ backend . clientId
    liftIO $ act uuid $ Just guiID

openFile :: FilePath -> Command State ()
openFile = withUUID . BatchCmd.openFile


dumpGraphViz :: Command State ()
dumpGraphViz = withWorkspace BatchCmd.dumpGraphViz


getProgram :: Maybe (GraphLocation, LocationSettings) -> Command State ()
getProgram = withWorkspace . BatchCmd.getProgram


addConnection :: Either OutPortRef NodeLoc -> Either AnyPortRef NodeLoc -> Command State ()
addConnection src dst = do
    let nl = case dst of
            Left (OutPortRef' (OutPortRef nl' _)) -> nl'
            Left (InPortRef'  (InPortRef  nl' _)) -> nl'
            Right nl'                             -> nl'
    collaborativeModify [nl]
    withWorkspace $ BatchCmd.addConnection src dst

addNode :: NodeLoc -> Text -> NodeMeta -> Maybe NodeLoc -> Command State ()
addNode nl expr nm connectTo = withWorkspace $ BatchCmd.addNode nl expr nm connectTo

addPort :: OutPortRef -> Maybe InPortRef -> Command State ()
addPort = withWorkspace .: BatchCmd.addPort

addSubgraph :: [ExpressionNode] -> [(OutPortRef, InPortRef)] -> Command State ()
addSubgraph [] []       = return ()
addSubgraph nodes conns = withWorkspace $ BatchCmd.addSubgraph (convert <$> nodes) (convert <$> conns)

autolayoutNodes :: [NodeLoc] -> Command State ()
autolayoutNodes []  = return ()
autolayoutNodes nls = withWorkspace $ BatchCmd.autolayoutNodes nls

collapseToFunction :: [NodeLoc] -> Command State ()
collapseToFunction []  = return ()
collapseToFunction nls = withWorkspace $ BatchCmd.collapseToFunction nls

copy :: [NodeLoc] -> Command State ()
copy []  = return ()
copy nls = withWorkspace $ BatchCmd.copy nls

getSubgraph :: NodeLoc -> Command State ()
getSubgraph nl = withWorkspace (BatchCmd.getSubgraph nl)

movePort :: OutPortRef -> Int -> Command State ()
movePort = withWorkspace .: BatchCmd.movePort

redo :: Command State ()
redo = withUUID BatchCmd.redo

removeConnection :: ConnectionId -> Command State ()
removeConnection connId = do
    collaborativeModify [connId ^. dstNodeLoc]
    withWorkspace $ BatchCmd.removeConnection connId

removeNodes :: [NodeLoc] -> Command State ()
removeNodes []  = return ()
removeNodes nls = withWorkspace $ BatchCmd.removeNodes nls

removePort :: OutPortRef -> Command State ()
removePort = withWorkspace . BatchCmd.removePort

renameNode :: NodeLoc -> Text -> Command State ()
renameNode = withWorkspace .:  BatchCmd.renameNode

renamePort :: OutPortRef -> Text -> Command State ()
renamePort = withWorkspace .: BatchCmd.renamePort

paste :: Position -> String -> Command State ()
paste = withWorkspace .: BatchCmd.paste

saveSettings :: LocationSettings -> Command State ()
saveSettings = withWorkspace . BatchCmd.saveSettings

searchNodes :: Command State ()
searchNodes = withWorkspace BatchCmd.searchNodes

setNodeExpression :: NodeLoc -> Text -> Command State ()
setNodeExpression = withWorkspace .: BatchCmd.setNodeExpression

setNodesMeta :: [(NodeLoc, NodeMeta)] -> Command State ()
setNodesMeta []      = return ()
setNodesMeta updates = withWorkspace $ BatchCmd.setNodesMeta updates

sendNodesMetaUpdate :: [(NodeLoc, NodeMeta)] -> Command State ()
sendNodesMetaUpdate []      = return ()
sendNodesMetaUpdate updates = withWorkspace $ BatchCmd.sendNodesMetaUpdate updates

setPortDefault :: InPortRef -> PortDefault -> Command State ()
setPortDefault portRef portDefault = do
    collaborativeModify [portRef ^. nodeLoc]
    withWorkspace $ BatchCmd.setPortDefault portRef portDefault

undo :: Command State ()
undo = withUUID BatchCmd.undo


requestCollaborationRefresh :: Command State ()
requestCollaborationRefresh = do
    clId <- use $ backend . clientId
    withWorkspace' $ BatchCmd.requestCollaborationRefresh clId

collaborativeTouch :: [NodeLoc] -> Command State ()
collaborativeTouch nodeLocs = unless (null nodeLocs) $ do
    clId <- use $ backend . clientId
    withWorkspace' $ BatchCmd.collaborativeTouch clId nodeLocs

collaborativeModify :: [NodeLoc] -> Command State ()
collaborativeModify nodeLocs = unless (null nodeLocs) $ do
    clId <- use $ backend . clientId
    withWorkspace' $ BatchCmd.collaborativeModify clId nodeLocs

cancelCollaborativeTouch :: [NodeLoc] -> Command State ()
cancelCollaborativeTouch nodeLocs = unless (null nodeLocs) $ do
    clId <- use $ backend . clientId
    withWorkspace' $ BatchCmd.cancelCollaborativeTouch clId nodeLocs
