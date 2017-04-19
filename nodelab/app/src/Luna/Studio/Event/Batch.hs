{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Batch where

import           Data.Aeson                             (ToJSON)
import           Luna.Studio.Prelude

import qualified Empire.API.Control.EmpireStarted       as EmpireStarted
import qualified Empire.API.Graph.AddConnection         as AddConnection
import qualified Empire.API.Graph.AddNode               as AddNode
import qualified Empire.API.Graph.AddPort               as AddPort
import qualified Empire.API.Graph.AddSubgraph           as AddSubgraph
import qualified Empire.API.Graph.CollaborationUpdate   as CollaborationUpdate
import qualified Empire.API.Graph.ConnectUpdate         as ConnectUpdate
import qualified Empire.API.Graph.DumpGraphViz          as DumpGraphViz
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Graph.GetSubgraphs          as GetSubgraphs
import qualified Empire.API.Graph.MonadsUpdate          as MonadsUpdate
import qualified Empire.API.Graph.MovePort              as MovePort
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResultUpdate
import qualified Empire.API.Graph.NodesUpdate           as NodesUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate
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
import qualified Empire.API.Graph.TypeCheck             as TypeCheck
import qualified Empire.API.Graph.Undo                  as Undo
import qualified Empire.API.Project.CreateProject       as CreateProject
import qualified Empire.API.Project.ExportProject       as ExportProject
import qualified Empire.API.Project.ImportProject       as ImportProject
import qualified Empire.API.Project.ListProjects        as ListProjects
import qualified Empire.API.Project.OpenProject         as OpenProject


data Event = UnknownEvent String
           | GetProgramResponse                     GetProgram.Response
           | AddConnectionResponse               AddConnection.Response
           | AddNodeResponse                           AddNode.Response
           | AddPortResponse                           AddPort.Response
           | AddSubgraphResponse                   AddSubgraph.Response
           | CollaborationUpdate           CollaborationUpdate.Update
           | ConnectionDropped
           | ConnectionOpened
           | ConnectUpdate                       ConnectUpdate.Update
           | DumpGraphVizResponse                 DumpGraphViz.Response
           | EmpireStarted                       EmpireStarted.Status
           | GetSubgraphsResponse                 GetSubgraphs.Response
           | MonadsUpdate                         MonadsUpdate.Update
           | MovePortResponse                         MovePort.Response
           | NodeResultUpdate                 NodeResultUpdate.Update
           | NodesUpdate                           NodesUpdate.Update
           | NodeTypecheckerUpdate                NodeTCUpdate.Update
           | RedoResponse                                 Redo.Response
           | RemoveConnectionResponse         RemoveConnection.Response
           | RemoveConnectionUpdate           RemoveConnection.Update
           | RemoveNodesResponse                   RemoveNodes.Response
           | RemovePortResponse                     RemovePort.Response
           | RenameNodeResponse                     RenameNode.Response
           | RenamePortResponse                     RenamePort.Response
           | SearchNodesResponse                   SearchNodes.Response
           | SetNodeCodeResponse                   SetNodeCode.Response
           | SetNodeExpressionResponse       SetNodeExpression.Response
           | SetNodesMetaResponse                 SetNodesMeta.Response
           | SetPortDefaultResponse             SetPortDefault.Response
           | TypeCheckResponse                       TypeCheck.Response
           | UndoResponse                                 Undo.Response

           | ProjectCreated                      CreateProject.Response
           | ProjectCreatedUpdate                CreateProject.Update
           | ProjectExported                     ExportProject.Response
           | ProjectImported                     ImportProject.Response
           | ProjectList                          ListProjects.Response
           | ProjectOpened                         OpenProject.Response
           | ProjectOpenedUpdate                   OpenProject.Update
           deriving (Eq, Show, Generic, NFData)

instance ToJSON Event
