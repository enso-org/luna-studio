module NodeEditor.Action.Batch  where

import           Common.Prelude
import           Data.UUID.Types                     (UUID)
import           Empire.API.Data.NodeMeta            (NodeMeta (NodeMeta))
import           Empire.API.Data.PortDefault         (PortDefault)
import           Empire.API.Data.PortRef             (AnyPortRef (InPortRef', OutPortRef'), InPortRef (InPortRef), OutPortRef (OutPortRef),
                                                      dstNodeLoc, nodeLoc)
import           Empire.API.Data.Position            (Position)
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

openFile :: FilePath -> Command State ()
openFile = withUUID . BatchCmd.openFile


dumpGraphViz :: Command State ()
dumpGraphViz = withWorkspace BatchCmd.dumpGraphViz


getProgram :: Command State ()
getProgram = withWorkspace BatchCmd.getProgram


addConnection :: Either OutPortRef NodeLoc -> Either AnyPortRef NodeLoc -> Command State ()
addConnection src dst = do
    let nl = case dst of
            Left (OutPortRef' (OutPortRef nl' _)) -> nl'
            Left (InPortRef'  (InPortRef  nl' _)) -> nl'
            Right nl'                             -> nl'
    collaborativeModify [nl]
    withWorkspace $ BatchCmd.addConnection src dst

addNode :: NodeLoc -> Text -> Position -> Bool -> Maybe NodeLoc -> Command State ()
addNode nl expr pos dispRes connectTo = withWorkspace $ BatchCmd.addNode nl expr (NodeMeta pos dispRes) connectTo

addPort :: OutPortRef -> Command State ()
addPort = withWorkspace . BatchCmd.addPort

addSubgraph :: [ExpressionNode] -> [(OutPortRef, InPortRef)] -> Command State ()
addSubgraph nodes conns = withWorkspace $ BatchCmd.addSubgraph (convert <$> nodes) (convert <$> conns)

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
removeNodes = withWorkspace . BatchCmd.removeNodes

removePort :: OutPortRef -> Command State ()
removePort = withWorkspace . BatchCmd.removePort

renameNode :: NodeLoc -> Text -> Command State ()
renameNode = withWorkspace .:  BatchCmd.renameNode

renamePort :: OutPortRef -> String -> Command State ()
renamePort = withWorkspace .: BatchCmd.renamePort

searchNodes :: Text -> (Int, Int) -> Command State ()
searchNodes = withWorkspace .: BatchCmd.searchNodes

setNodeCode :: NodeLoc -> Text -> Command State ()
setNodeCode = withWorkspace .:  BatchCmd.setNodeCode

setNodeExpression :: NodeLoc -> Text -> Command State ()
setNodeExpression = withWorkspace .: BatchCmd.setNodeExpression

setNodesMeta :: [(NodeLoc, Position, Bool)] -> Command State ()
setNodesMeta = withWorkspace . BatchCmd.setNodesMeta . map (\(nl, pos, dis) -> (nl, NodeMeta pos dis))

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
