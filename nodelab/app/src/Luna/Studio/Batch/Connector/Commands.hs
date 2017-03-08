module Luna.Studio.Batch.Connector.Commands where

import           Luna.Studio.Prelude

import qualified Data.Text                              as Text
import           Data.UUID.Types                        (UUID)

import           Luna.Studio.Batch.Connector.Connection (Message (..), sendRequest, sendUpdate)
import           Luna.Studio.Batch.Workspace            (Workspace)
import qualified Luna.Studio.Batch.Workspace            as Workspace

import           Empire.API.Data.Connection             (Connection)
import qualified Empire.API.Data.DefaultValue           as DefaultValue
import           Empire.API.Data.GraphLocation          (GraphLocation)
import qualified Empire.API.Data.GraphLocation          as GraphLocation
import           Empire.API.Data.Node                   (Node, NodeId)
import           Empire.API.Data.NodeMeta               (NodeMeta)
import           Empire.API.Data.PortRef                (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           Empire.API.Data.Project                (ProjectId)

import qualified Empire.API.Graph.AddNode               as AddNode
import qualified Empire.API.Graph.AddPort               as AddPort
import qualified Empire.API.Graph.AddSubgraph           as AddSubgraph
import qualified Empire.API.Graph.Collaboration         as Collaboration
import qualified Empire.API.Graph.Connect               as Connect
import qualified Empire.API.Graph.Disconnect            as Disconnect
import qualified Empire.API.Graph.DumpGraphViz          as DumpGraphViz
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Graph.MovePort              as MovePort
import qualified Empire.API.Graph.NodeSearch            as NodeSearch
import qualified Empire.API.Graph.Redo                  as Redo
import qualified Empire.API.Graph.RemoveNodes           as RemoveNodes
import qualified Empire.API.Graph.RemovePort            as RemovePort
import qualified Empire.API.Graph.RenameNode            as RenameNode
import qualified Empire.API.Graph.RenamePort            as RenamePort
import qualified Empire.API.Graph.SetCode               as SetCode
import qualified Empire.API.Graph.SetDefaultValue       as SetDefaultValue
import qualified Empire.API.Graph.Undo                  as Undo
import qualified Empire.API.Graph.UpdateNodeExpression  as UpdateNodeExpression
import qualified Empire.API.Graph.UpdateNodeMeta        as UpdateNodeMeta
import qualified Empire.API.Library.CreateLibrary       as CreateLibrary
import qualified Empire.API.Library.ListLibraries       as ListLibraries
import qualified Empire.API.Project.CreateProject       as CreateProject
import qualified Empire.API.Project.ExportProject       as ExportProject
import qualified Empire.API.Project.ImportProject       as ImportProject
import qualified Empire.API.Project.ListProjects        as ListProjects
import qualified Empire.API.Project.OpenProject         as OpenProject


withLibrary :: Workspace -> (GraphLocation -> a) -> a
withLibrary w f = f (w ^. Workspace.currentLocation)

addNode :: NodeId -> Text -> NodeMeta -> Maybe NodeId -> Workspace -> UUID -> Maybe UUID -> IO ()
addNode nodeId expression meta connectTo workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace AddNode.Request) nodeId expression meta connectTo

addPort :: NodeId -> Int -> Workspace -> UUID -> Maybe UUID -> IO ()
addPort nodeId pos workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace AddPort.Request) nodeId pos

addSubgraph :: [Node] -> [Connection] -> Workspace -> UUID -> Maybe UUID -> IO ()
addSubgraph nodes connections workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace AddSubgraph.Request) nodes connections

createProject :: Text -> UUID -> Maybe UUID -> IO ()
createProject name uuid guiID = sendRequest $ Message uuid guiID $ CreateProject.Request $ Text.unpack name

openProject :: FilePath -> UUID -> Maybe UUID -> IO ()
openProject path uuid guiID = sendRequest $ Message uuid guiID $ OpenProject.Request path

listProjects :: UUID -> Maybe UUID -> IO ()
listProjects uuid guiID = sendRequest $ Message uuid guiID ListProjects.Request

createLibrary :: Text -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
createLibrary name path workspace uuid guiID= sendRequest $ Message uuid guiID $ CreateLibrary.Request (workspace ^. Workspace.currentLocation . GraphLocation.projectId)
                                                                                  (Just $ Text.unpack name)
                                                                                  (Text.unpack path)
listLibraries :: ProjectId -> UUID -> Maybe UUID -> IO ()
listLibraries projectId uuid guiID = sendRequest $ Message uuid guiID $ ListLibraries.Request projectId

getProgram :: Workspace -> UUID -> Maybe UUID -> IO ()
getProgram workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace GetProgram.Request

nodeSearch :: Text -> (Int, Int) -> Workspace -> UUID -> Maybe UUID -> IO ()
nodeSearch query cursor workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace $ NodeSearch.Request query cursor

updateNodeExpression :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
updateNodeExpression nodeId expression workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace UpdateNodeExpression.Request nodeId expression

updateNodeMeta :: [(NodeId, NodeMeta)] -> Workspace -> UUID -> Maybe UUID -> IO ()
updateNodeMeta updates workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace UpdateNodeMeta.Request updates

renameNode :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
renameNode nid name w uuid guiID = sendRequest $ Message uuid guiID $ withLibrary w RenameNode.Request nid name

connect :: Either OutPortRef NodeId -> Either InPortRef NodeId -> Workspace -> UUID -> Maybe UUID -> IO ()
connect src dst workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace Connect.Request src dst

disconnect :: InPortRef -> Workspace -> UUID -> Maybe UUID -> IO ()
disconnect dst workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace Disconnect.Request dst

setCode :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
setCode nid newCode w uuid guiID = sendRequest $ Message uuid guiID $ withLibrary w SetCode.Request nid newCode

removeNodes :: [NodeId] -> Workspace -> UUID -> Maybe UUID ->  IO ()
removeNodes nodeIds workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace RemoveNodes.Request nodeIds

renamePort :: AnyPortRef -> String -> Workspace -> UUID -> Maybe UUID -> IO ()
renamePort portRef name w uuid guiID = sendRequest $ Message uuid guiID $ withLibrary w RenamePort.Request portRef name

movePort :: AnyPortRef -> Int -> Workspace -> UUID -> Maybe UUID -> IO ()
movePort portRef newPos workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace MovePort.Request) portRef newPos

removePort :: AnyPortRef -> Workspace -> UUID -> Maybe UUID -> IO ()
removePort portRef workspace uuid guiID = sendRequest $ Message uuid guiID $ (withLibrary workspace RemovePort.Request) portRef

setDefaultValue :: AnyPortRef -> DefaultValue.PortDefault -> Workspace -> UUID -> Maybe UUID -> IO ()
setDefaultValue portRef val workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace SetDefaultValue.Request portRef val

-- TODO[LJK, PM]: Probably remove
-- setInputNodeType :: NodeId -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
-- setInputNodeType nodeId tpe workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace SetInputNodeType.Request nodeId (convert tpe)

requestCollaborationRefresh :: Collaboration.ClientId -> Workspace -> IO ()
requestCollaborationRefresh clientId workspace = sendUpdate $ withLibrary workspace Collaboration.Update clientId Collaboration.Refresh

collaborativeTouch :: Collaboration.ClientId -> [NodeId] -> Workspace -> IO ()
collaborativeTouch clientId ids workspace = sendUpdate $ withLibrary workspace Collaboration.Update clientId  $ Collaboration.Touch ids

collaborativeModify :: Collaboration.ClientId ->[NodeId] -> Workspace -> IO ()
collaborativeModify clientId ids workspace = sendUpdate $ withLibrary workspace Collaboration.Update clientId  $ Collaboration.Modify ids

cancelCollaborativeTouch :: Collaboration.ClientId -> [NodeId] -> Workspace -> IO ()
cancelCollaborativeTouch clientId ids workspace = sendUpdate $ withLibrary workspace Collaboration.Update clientId $ Collaboration.CancelTouch ids

exportProject :: ProjectId -> UUID -> Maybe UUID -> IO ()
exportProject pid uuid guiID = sendRequest $ Message uuid guiID $ ExportProject.Request pid

importProject :: Text -> UUID -> Maybe UUID -> IO ()
importProject payload uuid guiID = sendRequest $ Message uuid guiID $ ImportProject.Request payload

dumpGraphViz :: Workspace -> UUID -> Maybe UUID -> IO ()
dumpGraphViz workspace uuid guiID = sendRequest $ Message uuid guiID $ withLibrary workspace DumpGraphViz.Request

requestUndo :: UUID -> Maybe UUID -> IO ()
requestUndo uuid guiID = sendRequest $ Message uuid guiID $ Undo.Request Undo.UndoRequest

requestRedo :: UUID -> Maybe UUID -> IO ()
requestRedo uuid guiID = sendRequest $ Message uuid guiID $ Redo.Request Redo.RedoRequest
