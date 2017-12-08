{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Event.Batch where

import           Common.Prelude

import           Common.Analytics                           (IsTrackedEvent (isTracked))
import           Common.Data.Event                          (EventName)
import qualified LunaStudio.API.Atom.MoveProject            as MoveProject
import qualified LunaStudio.API.Atom.Paste                  as AtomPaste
import qualified LunaStudio.API.Atom.Substitute             as Substitute
import qualified LunaStudio.API.Control.EmpireStarted       as EmpireStarted
import qualified LunaStudio.API.Graph.AddConnection         as AddConnection
import qualified LunaStudio.API.Graph.AddNode               as AddNode
import qualified LunaStudio.API.Graph.AddPort               as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph           as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes       as AutolayoutNodes
import qualified LunaStudio.API.Graph.CollaborationUpdate   as CollaborationUpdate
import qualified LunaStudio.API.Graph.CollapseToFunction    as CollapseToFunction
import qualified LunaStudio.API.Graph.Copy                  as Copy
import qualified LunaStudio.API.Graph.DumpGraphViz          as DumpGraphViz
import qualified LunaStudio.API.Graph.GetProgram            as GetProgram
import qualified LunaStudio.API.Graph.GetSubgraphs          as GetSubgraphs
import qualified LunaStudio.API.Graph.MonadsUpdate          as MonadsUpdate
import qualified LunaStudio.API.Graph.MovePort              as MovePort
import qualified LunaStudio.API.Graph.NodeResultUpdate      as NodeResultUpdate
import qualified LunaStudio.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate
import qualified LunaStudio.API.Graph.Paste                 as Paste
import qualified LunaStudio.API.Graph.Redo                  as Redo
import qualified LunaStudio.API.Graph.RemoveConnection      as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes           as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort            as RemovePort
import qualified LunaStudio.API.Graph.RenameNode            as RenameNode
import qualified LunaStudio.API.Graph.RenamePort            as RenamePort
import qualified LunaStudio.API.Graph.SearchNodes           as SearchNodes
import qualified LunaStudio.API.Graph.SetNodeExpression     as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta          as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault        as SetPortDefault
import qualified LunaStudio.API.Graph.TypeCheck             as TypeCheck
import qualified LunaStudio.API.Graph.Undo                  as Undo


data Event = UnknownEvent                             String
           | AddConnectionResponse             AddConnection.Response
           | AddNodeResponse                         AddNode.Response
           | AddPortResponse                         AddPort.Response
           | AddSubgraphResponse                 AddSubgraph.Response
           | AtomPasteResponse                     AtomPaste.Response
           | AutolayoutNodesResponse         AutolayoutNodes.Response
           | CollaborationUpdate         CollaborationUpdate.Update
           | CollapseToFunctionResponse   CollapseToFunction.Response
           | ConnectionDropped
           | ConnectionOpened
           | CopyResponse                               Copy.Response
           | DumpGraphVizResponse               DumpGraphViz.Response
           | EmpireStarted                     EmpireStarted.Status
           | GetProgramResponse                   GetProgram.Response
           | GetSubgraphsResponse               GetSubgraphs.Response
           | MonadsUpdate                       MonadsUpdate.Update
           | MovePortResponse                       MovePort.Response
           | NodeResultUpdate               NodeResultUpdate.Update
           | NodeTypecheckerUpdate              NodeTCUpdate.Update
           | PasteResponse                             Paste.Response
           | ProjectMoved                        MoveProject.Response
           | RedoResponse                               Redo.Response
           | RemoveConnectionResponse       RemoveConnection.Response
           | RemoveNodesResponse                 RemoveNodes.Response
           | RemovePortResponse                   RemovePort.Response
           | RenameNodeResponse                   RenameNode.Response
           | RenamePortResponse                   RenamePort.Response
           | SearchNodesResponse                 SearchNodes.Response
           | SetNodeExpressionResponse     SetNodeExpression.Response
           | SetNodesMetaResponse               SetNodesMeta.Response
           | SetPortDefaultResponse           SetPortDefault.Response
           | SubstituteResponse                   Substitute.Response
           | TypeCheckResponse                     TypeCheck.Response
           | UndoResponse                               Undo.Response
           deriving (Eq, Show, Generic, NFData)

instance EventName Event

instance IsTrackedEvent Event where
    isTracked (UnknownEvent _) = False
    isTracked e                = True
