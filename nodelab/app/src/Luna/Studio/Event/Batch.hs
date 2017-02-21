{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Batch where

import           Data.Aeson                             (ToJSON)
import           Luna.Studio.Prelude

import qualified Empire.API.Control.EmpireStarted       as EmpireStarted
import qualified Empire.API.Graph.AddNode               as AddNode
import qualified Empire.API.Graph.AddSubgraph           as AddSubgraph
import qualified Empire.API.Graph.CodeUpdate            as CodeUpdate
import qualified Empire.API.Graph.Collaboration         as Collaboration
import qualified Empire.API.Graph.Connect               as Connect
import qualified Empire.API.Graph.Disconnect            as Disconnect
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Graph.MonadsUpdate          as MonadsUpdate
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearch            as NodeSearch
import qualified Empire.API.Graph.NodesUpdate           as NodesUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate
import qualified Empire.API.Graph.RemoveNodes           as RemoveNodes
import qualified Empire.API.Graph.RenameNode            as RenameNode
import qualified Empire.API.Graph.UpdateNodeExpression  as UpdateNodeExpression
import qualified Empire.API.Graph.UpdateNodeMeta        as UpdateNodeMeta
import qualified Empire.API.Project.CreateProject       as CreateProject
import qualified Empire.API.Project.ExportProject       as ExportProject
import qualified Empire.API.Project.ImportProject       as ImportProject
import qualified Empire.API.Project.ListProjects        as ListProjects
import qualified Empire.API.Project.OpenProject         as OpenProject


data Event = UnknownEvent String
           | AddNodeResponse                           AddNode.Response
           | AddSubgraphResponse                   AddSubgraph.Response
           | CodeUpdated                            CodeUpdate.Update
           | CollaborationUpdate                 Collaboration.Update
           | ConnectionDropped
           | ConnectionOpened
           | ConnectResponse                           Connect.Response
           | DisconnectInverse                      Disconnect.Inverse
           | DisconnectResponse                     Disconnect.Response
           | EmpireStarted                       EmpireStarted.Status
           | NodeAdded                                 AddNode.Update
           | NodeMetaInverse                    UpdateNodeMeta.Inverse
           | NodeMetaResponse                   UpdateNodeMeta.Response
           | NodeMetaUpdated                    UpdateNodeMeta.Update
           | NodeRenamed                            RenameNode.Update
           | NodeRenameInverse                     RenameNode.Inverse
           | NodeRenameResponse                     RenameNode.Response
           | NodeResultUpdated                NodeResultUpdate.Update
           | NodesConnected                            Connect.Update
           | NodesDisconnected                      Disconnect.Update
           | NodeSearchResponse                     NodeSearch.Response
           | NodesRemoved                          RemoveNodes.Update
           | NodesUpdated                          NodesUpdate.Update
           | MonadsUpdated                        MonadsUpdate.Update
           | NodeTypechecked                      NodeTCUpdate.Update
           | ProgramFetched                         GetProgram.Response
           | ProjectCreated                      CreateProject.Response
           | ProjectCreatedUpdate                CreateProject.Update
           | ProjectExported                     ExportProject.Response
           | ProjectImported                     ImportProject.Response
           | ProjectList                          ListProjects.Response
           | ProjectOpened                         OpenProject.Response
           | ProjectOpenedUpdate                   OpenProject.Update
           | RemoveNodesInverse                    RemoveNodes.Inverse
           | RemoveNodesResponse                   RemoveNodes.Response
           | UpdateNodeExpressionResponse UpdateNodeExpression.Response
           deriving (Eq, Show, Generic, NFData)

instance ToJSON Event
