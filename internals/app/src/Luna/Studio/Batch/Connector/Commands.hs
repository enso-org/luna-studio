module Luna.Studio.Batch.Connector.Commands where

import qualified Data.Text                              as Text
import           Data.UUID.Types                        (UUID)
import qualified Empire.API.Atom.CloseFile              as CloseFile
import qualified Empire.API.Atom.GetBuffer              as GetBuffer
import qualified Empire.API.Atom.OpenFile               as OpenFile
import qualified Empire.API.Atom.SaveFile               as SaveFile
import qualified Empire.API.Atom.SetProject             as SetProject
import qualified Empire.API.Atom.Substitute             as Substitute
import qualified Empire.API.Data.Breadcrumb             as Breadcrumb
import           Empire.API.Data.Connection             (Connection, ConnectionId)
import           Empire.API.Data.GraphLocation          (GraphLocation)
import           Empire.API.Data.GraphLocation          (projectId)
import qualified Empire.API.Data.GraphLocation          as GraphLocation
import           Empire.API.Data.Node                   (Node, NodeId)
import           Empire.API.Data.NodeMeta               (NodeMeta)
import           Empire.API.Data.PortDefault            (PortDefault)
import           Empire.API.Data.PortRef                (AnyPortRef, InPortRef, OutPortRef)
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
import           Luna.Studio.Prelude


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

-- Atom requests --

closeFile :: FilePath -> UUID -> Maybe UUID -> IO ()
closeFile path uuid guiID = sendRequest $ Message uuid guiID $ CloseFile.Request path

getBuffer :: FilePath -> Maybe (Int, Int) -> UUID -> Maybe UUID -> IO ()
getBuffer path maybeSpan uuid guiID = sendRequest $ Message uuid guiID $ GetBuffer.Request path maybeSpan

openFile :: FilePath -> UUID -> Maybe UUID -> IO ()
openFile path uuid guiID = sendRequest $ Message uuid guiID $ OpenFile.Request path

saveFile :: FilePath -> UUID -> Maybe UUID -> IO ()
saveFile path uuid guiID = sendRequest $ Message uuid guiID $ SaveFile.Request path

setProject :: FilePath -> UUID -> Maybe UUID -> IO ()
setProject rootPath uuid guiID = sendRequest $ Message uuid guiID $ SetProject.Request rootPath

substitute :: FilePath -> Int -> Int -> Text -> Maybe Int -> UUID -> Maybe UUID -> IO ()
substitute path start end text cursor uuid guiID = sendRequest $ Message uuid guiID $ Substitute.Request path start end text cursor

--

dumpGraphViz :: Workspace -> UUID -> Maybe UUID -> IO ()
dumpGraphViz workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace DumpGraphViz.Request


getProgram :: Workspace -> UUID -> Maybe UUID -> IO ()
getProgram workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace GetProgram.Request

addConnection :: Either OutPortRef NodeId -> Either InPortRef NodeId -> Workspace -> UUID -> Maybe UUID -> IO ()
addConnection src dst workspace uuid guiID = undefined--sendRequest $ Message uuid guiID $ withLibrary workspace AddConnection.Request src dst

addNode :: NodeId -> Text -> NodeMeta -> Maybe NodeId -> Workspace -> UUID -> Maybe UUID -> IO ()
addNode nodeId expression meta connectTo workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace AddNode.Request) nodeId expression meta connectTo

addPort :: AnyPortRef -> Workspace -> UUID -> Maybe UUID -> IO ()
addPort portRef workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace AddPort.Request) portRef

addSubgraph :: [Node] -> [Connection] -> Workspace -> UUID -> Maybe UUID -> IO ()
addSubgraph nodes connections workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace AddSubgraph.Request) nodes connections

getSubgraph :: NodeId -> Workspace -> UUID -> Maybe UUID -> IO ()
getSubgraph nodeId workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace $ GetSubgraphs.Request . (GraphLocation.breadcrumb . Breadcrumb.items %~ (Breadcrumb.Lambda nodeId:))

movePort :: AnyPortRef -> AnyPortRef -> Workspace -> UUID -> Maybe UUID -> IO ()
movePort portRef newPortRef workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace MovePort.Request) portRef newPortRef

redo :: UUID -> Maybe UUID -> IO ()
redo uuid guiID = sendRequest $ Message uuid guiID $ Redo.Request Redo.RedoRequest

removeConnection :: ConnectionId -> Workspace -> UUID -> Maybe UUID -> IO ()
removeConnection connId workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace RemoveConnection.Request connId

removeNodes :: [NodeId] -> Workspace -> UUID -> Maybe UUID ->  IO ()
removeNodes nodeIds workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace RemoveNodes.Request nodeIds

removePort :: AnyPortRef -> Workspace -> UUID -> Maybe UUID -> IO ()
removePort portRef workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace RemovePort.Request) portRef

renameNode :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
renameNode nid name w uuid guiID = sendRequest $ Message uuid guiID $ withLibrary w RenameNode.Request nid name

renamePort :: AnyPortRef -> String -> Workspace -> UUID -> Maybe UUID -> IO ()
renamePort portRef name w uuid guiID = sendRequest $ Message uuid guiID $ withLibrary w RenamePort.Request portRef name

searchNodes :: Text -> (Int, Int) -> Workspace -> UUID -> Maybe UUID -> IO ()
searchNodes query cursor workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace $ SearchNodes.Request query cursor

-- TODO[LJK, PM]: Probably remove
-- setInputNodeType :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
-- setInputNodeType nodeId tpe workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace SetInputNodeType.Request nodeId (convert tpe)

setNodeCode :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
setNodeCode nid newCode w uuid guiID = sendRequest $ Message uuid guiID $ withLibrary w SetNodeCode.Request nid newCode

setNodeExpression :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
setNodeExpression nodeId expression workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace SetNodeExpression.Request nodeId expression

setNodesMeta :: [(NodeId, NodeMeta)] -> Workspace -> UUID -> Maybe UUID -> IO ()
setNodesMeta updates workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace SetNodesMeta.Request updates

setPortDefault :: AnyPortRef -> PortDefault -> Workspace -> UUID -> Maybe UUID -> IO ()
setPortDefault portRef portDef workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace SetPortDefault.Request portRef portDef

undo :: UUID -> Maybe UUID -> IO ()
undo uuid guiID = sendRequest $ Message uuid guiID $ Undo.Request Undo.UndoRequest



requestCollaborationRefresh :: ClientId -> Workspace -> IO ()
requestCollaborationRefresh clientId workspace = sendUpdate $ withLibrary workspace CollaborationUpdate.Update clientId CollaborationUpdate.Refresh

collaborativeTouch :: ClientId -> [NodeId] -> Workspace -> IO ()
collaborativeTouch clientId ids workspace = sendUpdate $ withLibrary workspace CollaborationUpdate.Update clientId  $ CollaborationUpdate.Touch ids

collaborativeModify :: ClientId ->[NodeId] -> Workspace -> IO ()
collaborativeModify clientId ids workspace = sendUpdate $ withLibrary workspace CollaborationUpdate.Update clientId  $ CollaborationUpdate.Modify ids

cancelCollaborativeTouch :: ClientId -> [NodeId] -> Workspace -> IO ()
cancelCollaborativeTouch clientId ids workspace = sendUpdate $ withLibrary workspace CollaborationUpdate.Update clientId $ CollaborationUpdate.CancelTouch ids
