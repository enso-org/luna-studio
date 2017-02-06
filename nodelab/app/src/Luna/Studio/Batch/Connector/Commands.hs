module Luna.Studio.Batch.Connector.Commands where

import           Luna.Studio.Prelude

import           Data.UUID.Types                        (UUID)

import           Luna.Studio.Batch.Connector.Connection (sendRequest, sendUpdate)
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
import qualified Empire.API.Graph.AddSubgraph           as AddSubgraph
import qualified Empire.API.Graph.Collaboration         as Collaboration
import qualified Empire.API.Graph.Connect               as Connect
import qualified Empire.API.Graph.Disconnect            as Disconnect
import qualified Empire.API.Graph.DumpGraphViz          as DumpGraphViz
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Graph.RemoveNodes           as RemoveNodes
import qualified Empire.API.Graph.RenameNode            as RenameNode
import qualified Empire.API.Graph.SetCode               as SetCode
import qualified Empire.API.Graph.SetDefaultValue       as SetDefaultValue
import qualified Empire.API.Graph.SetInputNodeType      as SetInputNodeType
import qualified Empire.API.Graph.UpdateNodeExpression  as UpdateNodeExpression
import qualified Empire.API.Graph.UpdateNodeMeta        as UpdateNodeMeta
import qualified Empire.API.Library.CreateLibrary       as CreateLibrary
import qualified Empire.API.Library.ListLibraries       as ListLibraries
import qualified Empire.API.Project.CreateProject       as CreateProject
import qualified Empire.API.Project.ExportProject       as ExportProject
import qualified Empire.API.Project.ImportProject       as ImportProject
import qualified Empire.API.Project.ListProjects        as ListProjects


withLibrary :: Workspace -> (GraphLocation -> a) -> a
withLibrary w f = f (w ^. Workspace.currentLocation)

addNode :: Text -> NodeMeta -> Maybe NodeId -> Workspace -> UUID -> IO ()
addNode expression meta connectTo workspace uuid = sendRequest uuid $ withLibrary workspace AddNode.Request (AddNode.ExpressionNode expression) meta connectTo

addSubgraph :: [Node] -> [Connection] -> Workspace -> UUID -> IO ()
addSubgraph nodes connections workspace uuid = sendRequest uuid $ withLibrary workspace AddSubgraph.Request nodes connections

createProject :: Text -> UUID -> IO ()
createProject name uuid = sendRequest uuid $ CreateProject.Request $ convert name

listProjects :: UUID -> IO ()
listProjects uuid = sendRequest uuid ListProjects.Request

createLibrary :: Text -> Text -> Workspace -> UUID -> IO ()
createLibrary name path workspace uuid = sendRequest uuid $ CreateLibrary.Request (workspace ^. Workspace.currentLocation . GraphLocation.projectId)
                                                                                  (Just $ convert name)
                                                                                  (convert path)
listLibraries :: ProjectId -> UUID -> IO ()
listLibraries projectId uuid = sendRequest uuid $ ListLibraries.Request projectId

getProgram :: Workspace -> UUID -> IO ()
getProgram workspace uuid = sendRequest uuid $ withLibrary workspace GetProgram.Request

updateNodeExpression :: NodeId -> Text -> Workspace -> UUID -> IO ()
updateNodeExpression nodeId expression workspace uuid = sendRequest uuid $ withLibrary workspace UpdateNodeExpression.Request nodeId expression

updateNodeMeta :: [(NodeId, NodeMeta)] -> Workspace -> UUID -> IO ()
updateNodeMeta updates workspace uuid = sendRequest uuid $ withLibrary workspace UpdateNodeMeta.Request updates

renameNode :: NodeId -> Text -> Workspace -> UUID -> IO ()
renameNode nid name w uuid = sendRequest uuid $ withLibrary w RenameNode.Request nid name

setCode :: NodeId -> Text -> Workspace -> UUID -> IO ()
setCode nid newCode w uuid = sendRequest uuid $ withLibrary w SetCode.Request nid newCode

removeNodes :: [NodeId] -> Workspace -> UUID ->  IO ()
removeNodes nodeIds workspace uuid = sendRequest uuid $ withLibrary workspace RemoveNodes.Request nodeIds

autoconnect :: NodeId -> NodeId -> Workspace -> UUID -> IO ()
autoconnect src dst workspace uuid = sendRequest uuid $ withLibrary workspace Connect.Request $ Connect.NodeConnection src dst

connectNodes :: OutPortRef -> InPortRef -> Workspace -> UUID -> IO ()
connectNodes src dst workspace uuid = sendRequest uuid $ withLibrary workspace Connect.Request $ Connect.PortConnection src dst

disconnectNodes :: InPortRef -> Workspace -> UUID -> IO ()
disconnectNodes dst workspace uuid = sendRequest uuid $ withLibrary workspace Disconnect.Request dst

setDefaultValue :: AnyPortRef -> DefaultValue.PortDefault -> Workspace -> UUID -> IO ()
setDefaultValue portRef val workspace uuid = sendRequest uuid $ withLibrary workspace SetDefaultValue.Request portRef val

setInputNodeType :: NodeId -> Text -> Workspace -> UUID -> IO ()
setInputNodeType nodeId tpe workspace uuid = sendRequest uuid $ withLibrary workspace SetInputNodeType.Request nodeId (convert tpe)

requestCollaborationRefresh :: Collaboration.ClientId -> Workspace -> IO ()
requestCollaborationRefresh clientId workspace = sendUpdate $ withLibrary workspace Collaboration.Update clientId Collaboration.Refresh

collaborativeTouch :: Collaboration.ClientId ->[NodeId] -> Workspace -> IO ()
collaborativeTouch clientId ids workspace = sendUpdate $ withLibrary workspace Collaboration.Update clientId  $ Collaboration.Touch ids

collaborativeModify :: Collaboration.ClientId ->[NodeId] -> Workspace -> IO ()
collaborativeModify clientId ids workspace = sendUpdate $ withLibrary workspace Collaboration.Update clientId  $ Collaboration.Modify ids

cancelCollaborativeTouch :: Collaboration.ClientId -> [NodeId] -> Workspace -> IO ()
cancelCollaborativeTouch clientId ids workspace = sendUpdate $ withLibrary workspace Collaboration.Update clientId $ Collaboration.CancelTouch ids

exportProject :: ProjectId -> UUID -> IO ()
exportProject pid uuid = sendRequest uuid $ ExportProject.Request pid

importProject :: Text -> UUID -> IO ()
importProject payload uuid = sendRequest uuid $ ImportProject.Request payload

dumpGraphViz :: Workspace -> UUID -> IO ()
dumpGraphViz workspace uuid = sendRequest uuid $ withLibrary workspace DumpGraphViz.Request
