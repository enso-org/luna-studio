module Luna.Studio.Batch.Connector.Commands where

import qualified Data.Text                              as Text
import           Data.UUID.Types                        (UUID)
import           Empire.API.Data.Connection             (Connection)
import           Empire.API.Data.GraphLocation          (GraphLocation)
import           Empire.API.Data.GraphLocation          (projectId)
import qualified Empire.API.Data.GraphLocation          as GraphLocation
import           Empire.API.Data.Node                   (Node)
import           Empire.API.Data.NodeLoc                (NodeLoc)
import qualified Empire.API.Data.NodeLoc                as NodeLoc
import           Empire.API.Data.NodeMeta               (NodeMeta)
import           Empire.API.Data.PortDefault            (PortDefault)
import           Empire.API.Data.Project                (ProjectId)
import qualified Empire.API.Graph.AddConnection         as AddConnection
import qualified Empire.API.Graph.AddNode               as AddNode
import qualified Empire.API.Graph.AddPort               as AddPort
import qualified Empire.API.Graph.AddSubgraph           as AddSubgraph
import           Empire.API.Graph.CollaborationUpdate   (ClientId)
import qualified Empire.API.Graph.CollaborationUpdate   as CollaborationUpdate
import qualified Empire.API.Graph.DumpGraphViz          as DumpGraphViz
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Graph.GetSubgraphs          as GetSubgraphs
import qualified Empire.API.Graph.MovePort              as MovePort
import qualified Empire.API.Graph.Redo                  as Redo
import qualified Empire.API.Graph.RemoveConnection      as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes           as RemoveNodes
import qualified Empire.API.Graph.RemovePort            as RemovePort
import qualified Empire.API.Graph.RenameNode            as RenameNode
import qualified Empire.API.Graph.RenamePort            as RenamePort
import qualified Empire.API.Graph.SearchNodes           as SearchNodes
import qualified Empire.API.Graph.SetNodeCode           as SetNodeCode
import qualified Empire.API.Graph.SetNodeExpression     as SetNodeExpression
import qualified Empire.API.Graph.SetNodesMeta          as SetNodesMeta
import qualified Empire.API.Graph.SetPortDefault        as SetPortDefault
import qualified Empire.API.Graph.Undo                  as Undo
import qualified Empire.API.Library.CreateLibrary       as CreateLibrary
import qualified Empire.API.Library.ListLibraries       as ListLibraries
import qualified Empire.API.Project.CreateProject       as CreateProject
import qualified Empire.API.Project.ExportProject       as ExportProject
import qualified Empire.API.Project.ImportProject       as ImportProject
import qualified Empire.API.Project.ListProjects        as ListProjects
import qualified Empire.API.Project.OpenProject         as OpenProject
import           Luna.Studio.Batch.Connector.Connection (Message (Message), sendRequest, sendUpdate)
import           Luna.Studio.Batch.Workspace            (Workspace)
import           Luna.Studio.Batch.Workspace            (currentLocation)
import           Luna.Studio.Data.PortRef               (AnyPortRef, OutPortRef)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection     (ConnectionId)

withLibrary :: Workspace -> (GraphLocation -> a) -> a
withLibrary w f = f $ w ^. currentLocation


createLibrary :: Text -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
createLibrary name path workspace uuid guiID= sendRequest $ Message uuid guiID $ CreateLibrary.Request (workspace ^. currentLocation . projectId) (Just $ Text.unpack name) (Text.unpack path)

listLibraries :: ProjectId -> UUID -> Maybe UUID -> IO ()
listLibraries pid uuid guiID = sendRequest $ Message uuid guiID $ ListLibraries.Request pid


createProject :: Text -> UUID -> Maybe UUID -> IO ()
createProject name uuid guiID = sendRequest $ Message uuid guiID $ CreateProject.Request $ Text.unpack name

exportProject :: ProjectId -> UUID -> Maybe UUID -> IO ()
exportProject pid uuid guiID = sendRequest $ Message uuid guiID $ ExportProject.Request pid

importProject :: Text -> UUID -> Maybe UUID -> IO ()
importProject payload uuid guiID = sendRequest $ Message uuid guiID $ ImportProject.Request payload

listProjects :: UUID -> Maybe UUID -> IO ()
listProjects uuid guiID = sendRequest $ Message uuid guiID ListProjects.Request

openProject :: FilePath -> UUID -> Maybe UUID -> IO ()
openProject path uuid guiID = sendRequest $ Message uuid guiID $ OpenProject.Request path


dumpGraphViz :: Workspace -> UUID -> Maybe UUID -> IO ()
dumpGraphViz workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace DumpGraphViz.Request


getProgram :: Workspace -> UUID -> Maybe UUID -> IO ()
getProgram workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace GetProgram.Request

addConnection :: Either OutPortRef NodeLoc -> Either AnyPortRef NodeLoc -> Workspace -> UUID -> Maybe UUID -> IO ()
addConnection src dst workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace AddConnection.Request (conv src) (conv dst) where
    conv (Left a) = Left $ convert a
    conv (Right a) = Right $ a ^. NodeLoc.nodeId

addNode :: NodeLoc -> Text -> NodeMeta -> Maybe NodeLoc -> Workspace -> UUID -> Maybe UUID -> IO ()
addNode nodeLoc expression meta connectTo workspace uuid guiID =
    sendRequest $ Message uuid guiID $ (withLibrary workspace' AddNode.Request) nodeId expression meta (convert <$> connectTo) where
        (workspace', nodeId) = convert (workspace, nodeLoc)

addPort :: AnyPortRef -> Workspace -> UUID -> Maybe UUID -> IO ()
addPort portRef workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace AddPort.Request) $ convert portRef

addSubgraph :: [Node] -> [Connection] -> Workspace -> UUID -> Maybe UUID -> IO ()
addSubgraph nodes connections workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace AddSubgraph.Request) nodes connections

getSubgraph :: NodeLoc -> Workspace -> UUID -> Maybe UUID -> IO ()
getSubgraph nodeLoc workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace $ GetSubgraphs.Request . (GraphLocation.breadcrumb .~ convert (workspace, nodeLoc)) where

movePort :: AnyPortRef -> Int -> Workspace -> UUID -> Maybe UUID -> IO ()
movePort portRef newPortPos workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace' MovePort.Request) portRef' newPortPos where
    (workspace', portRef') = convert (workspace, portRef)

redo :: UUID -> Maybe UUID -> IO ()
redo uuid guiID = sendRequest $ Message uuid guiID $ Redo.Request Redo.RedoRequest

removeConnection :: ConnectionId -> Workspace -> UUID -> Maybe UUID -> IO ()
removeConnection connId workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace' RemoveConnection.Request connId' where
    (workspace', connId') = convert (workspace, connId)

removeNodes :: [NodeLoc] -> Workspace -> UUID -> Maybe UUID ->  IO ()
removeNodes nodeLocs workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace RemoveNodes.Request nodeLocs

removePort :: AnyPortRef -> Workspace -> UUID -> Maybe UUID -> IO ()
removePort portRef workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace' RemovePort.Request) portRef' where
    (workspace', portRef') = convert (workspace, portRef)

renameNode :: NodeLoc -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
renameNode nl name workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace' RenameNode.Request nodeId name where
    (workspace', nodeId) = convert (workspace, nl)

renamePort :: AnyPortRef -> String -> Workspace -> UUID -> Maybe UUID -> IO ()
renamePort portRef name workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace' RenamePort.Request portRef' name where
    (workspace', portRef') = convert (workspace, portRef)

searchNodes :: Text -> (Int, Int) -> Workspace -> UUID -> Maybe UUID -> IO ()
searchNodes query cursor workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace $ SearchNodes.Request query cursor

-- TODO[LJK, PM]: Probably remove
-- setInputNodeType :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
-- setInputNodeType nodeId tpe workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace SetInputNodeType.Request nodeId (convert tpe)

setNodeCode :: NodeLoc -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
setNodeCode nodeLoc newCode workspace uuid guiID =
    sendRequest $ Message uuid guiID $ withLibrary workspace' SetNodeCode.Request nodeId newCode where
        (workspace', nodeId) = convert (workspace, nodeLoc)

setNodeExpression :: NodeLoc -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
setNodeExpression nodeLoc expression workspace uuid guiID =
    sendRequest $ Message uuid guiID $ withLibrary workspace' SetNodeExpression.Request nodeId expression where
        (workspace', nodeId) = convert (workspace, nodeLoc)

setNodesMeta :: [(NodeLoc, NodeMeta)] -> Workspace -> UUID -> Maybe UUID -> IO ()
setNodesMeta updates workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace SetNodesMeta.Request (map (_1 %~ convert) updates)

setPortDefault :: AnyPortRef -> PortDefault -> Workspace -> UUID -> Maybe UUID -> IO ()
setPortDefault portRef portDef workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace' SetPortDefault.Request portRef' portDef where
    (workspace', portRef') = convert (workspace, portRef)

undo :: UUID -> Maybe UUID -> IO ()
undo uuid guiID = sendRequest $ Message uuid guiID $ Undo.Request Undo.UndoRequest



requestCollaborationRefresh :: ClientId -> Workspace -> IO ()
requestCollaborationRefresh clientId workspace = sendUpdate $ withLibrary workspace CollaborationUpdate.Update clientId CollaborationUpdate.Refresh

collaborativeTouch :: ClientId -> [NodeLoc] -> Workspace -> IO ()
collaborativeTouch clientId locs workspace = sendUpdate $ withLibrary workspace CollaborationUpdate.Update clientId  $ CollaborationUpdate.Touch locs

collaborativeModify :: ClientId -> [NodeLoc] -> Workspace -> IO ()
collaborativeModify clientId locs workspace = sendUpdate $ withLibrary workspace CollaborationUpdate.Update clientId  $ CollaborationUpdate.Modify locs

cancelCollaborativeTouch :: ClientId -> [NodeLoc] -> Workspace -> IO ()
cancelCollaborativeTouch clientId locs workspace = sendUpdate $ withLibrary workspace CollaborationUpdate.Update clientId $ CollaborationUpdate.CancelTouch locs
