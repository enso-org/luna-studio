module NodeEditor.Action.Batch  where

import Common.Prelude
import Prelude (error)

import qualified Data.Map                              as Map
import qualified Data.Set                              as Set
import qualified LunaStudio.API.Graph.AddConnection    as AddConnection
import qualified LunaStudio.API.Graph.SetNodesMeta     as SetNodesMeta
import qualified LunaStudio.Data.Searcher.Hint.Library as Library
import qualified NodeEditor.Batch.Connector.Commands   as BatchCmd

import Common.Action.Command             (Command)
import Data.Map                          (Map)
import Data.Set                          (Set)
import Data.UUID.Types                   (UUID)
import LunaStudio.Data.Connection        (Connection)
import LunaStudio.Data.GraphLocation     (GraphLocation)
import LunaStudio.Data.NodeMeta          (NodeMeta)
import LunaStudio.Data.PortDefault       (PortDefault)
import LunaStudio.Data.PortRef           (AnyPortRef (InPortRef', OutPortRef'),
                                          InPortRef (InPortRef),
                                          OutPortRef (OutPortRef), dstNodeLoc,
                                          nodeLoc)
import LunaStudio.Data.Position          (Position)
import LunaStudio.Data.Project           (LocationSettings)
import NodeEditor.Action.State.App       (getWorkspace)
import NodeEditor.Action.UUID            (registerRequest)
import NodeEditor.Batch.Workspace        (Workspace)
import NodeEditor.React.Model.Connection (ConnectionId)
import NodeEditor.React.Model.Node       (ExpressionNode, NodeLoc)
import NodeEditor.State.Global           (State, backend, clientId)


withWorkspace :: (Workspace -> UUID -> Maybe UUID -> IO ()) -> Command State ()
withWorkspace act = do
    uuid       <- registerRequest
    guiID      <- use $ backend . clientId
    withJustM getWorkspace $ \workspace' ->
        liftIO $ act workspace' uuid $ Just guiID

withMayWorkspace :: (Maybe Workspace -> UUID -> Maybe UUID -> IO ())
    -> Command State ()
withMayWorkspace act = do
    uuid       <- registerRequest
    guiID      <- use $ backend . clientId
    mayWorkspace <- getWorkspace
    liftIO $ act mayWorkspace uuid $ Just guiID

withWorkspace' :: (Workspace -> IO ()) -> Command State ()
withWorkspace' act = withJustM getWorkspace $ liftIO . act

withUUID :: (UUID -> Maybe UUID -> IO ()) -> Command State ()
withUUID act = do
    uuid  <- registerRequest
    guiID <- use $ backend . clientId
    liftIO $ act uuid $ Just guiID

openFile :: FilePath -> Command State ()
openFile = withUUID . BatchCmd.openFile


dumpGraphViz :: Command State ()
dumpGraphViz = withWorkspace BatchCmd.dumpGraphViz


getProgram :: Maybe (GraphLocation, LocationSettings) -> Bool -> Command State ()
getProgram = withWorkspace .: BatchCmd.getProgram


addConnection :: Either OutPortRef NodeLoc -> Either AnyPortRef NodeLoc
    -> Command State ()
addConnection src dst = do
    let nl = case dst of
            Left (OutPortRef' (OutPortRef nl' _)) -> nl'
            Left (InPortRef'  (InPortRef  nl' _)) -> nl'
            Right nl'                             -> nl'
    collaborativeModify [nl]
    withWorkspace $ BatchCmd.addConnection src dst

addConnectionRequest :: Either OutPortRef NodeLoc -> Either AnyPortRef NodeLoc
    -> Command State AddConnection.Request
addConnectionRequest src dst = do
    let nl = case dst of
            Left (OutPortRef' (OutPortRef nl' _)) -> nl'
            Left (InPortRef'  (InPortRef  nl' _)) -> nl'
            Right nl'                             -> nl'
    collaborativeModify [nl]
    mayWorkspace <- getWorkspace
    case mayWorkspace of
        Just w -> pure $ BatchCmd.addConnectionRequest src dst w
        _      -> error "no workspace"

addImport :: Library.Name -> Command State ()
addImport = addImports . Set.singleton

addImports :: Set Library.Name -> Command State ()
addImports = withWorkspace . BatchCmd.addImports

addNode :: NodeLoc -> Text -> NodeMeta -> Maybe NodeLoc -> Command State ()
addNode nl expr nm connectTo
    = withWorkspace $ BatchCmd.addNode nl expr nm connectTo

addPort :: OutPortRef -> Maybe InPortRef -> Maybe Text -> Command State ()
addPort = withWorkspace .:. BatchCmd.addPort

addSubgraph :: [ExpressionNode] -> [Connection] -> Command State ()
addSubgraph [] []       = return ()
addSubgraph nodes conns
    = withWorkspace $ BatchCmd.addSubgraph (convert <$> nodes) conns

autolayoutNodes :: [NodeLoc] -> Bool -> Command State ()
autolayoutNodes []  _            = return ()
autolayoutNodes nls shouldCenter
    = withWorkspace $ BatchCmd.autolayoutNodes nls shouldCenter

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

moveNodeRequest :: Map NodeLoc NodeMeta -> Command State SetNodesMeta.Request
moveNodeRequest updates = do
    mayWorkspace <- getWorkspace
    case mayWorkspace of
        Just w -> pure $ BatchCmd.setNodesMetaRequest updates w
        _      -> error "no workspace"

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

searchNodes :: Set Library.Name -> Command State ()
searchNodes = withWorkspace . BatchCmd.searchNodes

setNodeExpression :: NodeLoc -> Text -> Command State ()
setNodeExpression = withWorkspace .: BatchCmd.setNodeExpression

setNodesMeta :: Map NodeLoc NodeMeta -> Command State ()
setNodesMeta updates = unless (Map.null updates) . withWorkspace
    $ BatchCmd.setNodesMeta updates

sendNodesMetaUpdate :: Map NodeLoc NodeMeta -> Command State ()
sendNodesMetaUpdate updates = unless (Map.null updates) . withWorkspace
    $ BatchCmd.sendNodesMetaUpdate updates

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
