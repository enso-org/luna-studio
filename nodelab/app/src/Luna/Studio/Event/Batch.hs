{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Batch where

import           Data.Aeson                             (ToJSON)
import           Luna.Studio.Prelude

import qualified Empire.API.Control.EmpireStarted       as EmpireStarted
import qualified Empire.API.Graph.AddNode               as AddNode
import qualified Empire.API.Graph.AddPort               as AddPort
import qualified Empire.API.Graph.AddSubgraph           as AddSubgraph
import qualified Empire.API.Graph.Code                  as Code
import qualified Empire.API.Graph.Collaboration         as Collaboration
import qualified Empire.API.Graph.Connect               as Connect
import qualified Empire.API.Graph.DumpGraphViz          as DumpGraphViz
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Graph.MonadsUpdate          as MonadsUpdate
import qualified Empire.API.Graph.MovePort              as MovePort
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearch            as NodeSearch
import qualified Empire.API.Graph.NodesUpdate           as NodesUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate
import qualified Empire.API.Graph.RemoveConnection      as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes           as RemoveNodes
import qualified Empire.API.Graph.RemovePort            as RemovePort
import qualified Empire.API.Graph.RenameNode            as RenameNode
import qualified Empire.API.Graph.RenamePort            as RenamePort
import qualified Empire.API.Graph.SetCode               as SetCode
import qualified Empire.API.Graph.UpdateNodeExpression  as UpdateNodeExpression
import qualified Empire.API.Graph.UpdateNodeMeta        as UpdateNodeMeta
import qualified Empire.API.Project.CreateProject       as CreateProject
import qualified Empire.API.Project.ExportProject       as ExportProject
import qualified Empire.API.Project.ImportProject       as ImportProject
import qualified Empire.API.Project.ListProjects        as ListProjects
import qualified Empire.API.Project.OpenProject         as OpenProject


data Event = UnknownEvent String
           | GetProgramResponse                     GetProgram.Response
           | AddNodeResponse                           AddNode.Response
           | AddPortResponse                           AddPort.Response
           | AddSubgraphResponse                   AddSubgraph.Response
           | CodeUpdate                                   Code.Update
           | CollaborationUpdate                 Collaboration.Update
           | ConnectionDropped
           | ConnectionOpened
           | ConnectResponse                           Connect.Response
           | ConnectUpdate                             Connect.Update
           | DumpGraphVizResponse                 DumpGraphViz.Response
           | EmpireStarted                       EmpireStarted.Status
           | MonadsUpdate                         MonadsUpdate.Update
           | MovePortResponse                         MovePort.Response
           | NodeResultUpdate                 NodeResultUpdate.Update
           | NodeSearchResponse                     NodeSearch.Response

           | RemoveConnectionResponse         RemoveConnection.Response
           | RemoveConnectionUpdate           RemoveConnection.Update

           | NodeCodeSet                               SetCode.Update
           | NodeMetaInverse                    UpdateNodeMeta.Inverse
           | NodeMetaResponse                   UpdateNodeMeta.Response
           | NodeMetaUpdated                    UpdateNodeMeta.Update
           | NodeRenamed                            RenameNode.Update
           | PortRenamed                            RenamePort.Update
           | NodeRenameResponse                     RenameNode.Response
           | NodesRemoved                          RemoveNodes.Update
           | NodesUpdated                          NodesUpdate.Update
           | NodeTypechecked                      NodeTCUpdate.Update
           | ProjectCreated                      CreateProject.Response
           | ProjectCreatedUpdate                CreateProject.Update
           | ProjectExported                     ExportProject.Response
           | ProjectImported                     ImportProject.Response
           | ProjectList                          ListProjects.Response
           | ProjectOpened                         OpenProject.Response
           | ProjectOpenedUpdate                   OpenProject.Update
           | RemoveNodesInverse                    RemoveNodes.Inverse
           | RemoveNodesResponse                   RemoveNodes.Response
           | RemovePortResponse                     RemovePort.Response
           | UpdateNodeExpressionResponse UpdateNodeExpression.Response
           deriving (Eq, Show, Generic, NFData)

instance ToJSON Event
