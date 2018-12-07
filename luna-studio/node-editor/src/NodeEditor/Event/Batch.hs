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
import qualified LunaStudio.API.Graph.AddImports            as AddImports
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
import qualified LunaStudio.API.Graph.SetCode               as SetCode
import qualified LunaStudio.API.Graph.SetNodeExpression     as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta          as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault        as SetPortDefault
import qualified LunaStudio.API.Graph.Transaction           as Transaction
import qualified LunaStudio.API.Graph.TypeCheck             as TypeCheck
import qualified LunaStudio.API.Graph.Undo                  as Undo

import LunaStudio.API.Response (ResponseOf)


data Event = UnknownEvent                                                String
           | AddConnectionResponse           (ResponseOf AddConnection.Request)
           | AddImportsResponse                 (ResponseOf AddImports.Request)
           | AddNodeResponse                       (ResponseOf AddNode.Request)
           | AddPortResponse                       (ResponseOf AddPort.Request)
           | AddSubgraphResponse               (ResponseOf AddSubgraph.Request)
           | AtomPasteResponse                   (ResponseOf AtomPaste.Request)
           | AutolayoutNodesResponse       (ResponseOf AutolayoutNodes.Request)
           | CollaborationUpdate                   CollaborationUpdate.Update
           | CollapseToFunctionResponse (ResponseOf CollapseToFunction.Request)
           | ConnectionDropped
           | ConnectionOpened
           | CopyResponse                             (ResponseOf Copy.Request)
           | DumpGraphVizResponse             (ResponseOf DumpGraphViz.Request)
           | EmpireStarted                               EmpireStarted.Status
           | GetProgramResponse                 (ResponseOf GetProgram.Request)
           | GetSubgraphsResponse             (ResponseOf GetSubgraphs.Request)
           | MonadsUpdate                                 MonadsUpdate.Update
           | MovePortResponse                     (ResponseOf MovePort.Request)
           | NodeResultUpdate                         NodeResultUpdate.Update
           | NodeTypecheckerUpdate                        NodeTCUpdate.Update
           | PasteResponse                           (ResponseOf Paste.Request)
           | ProjectMoved                                  MoveProject.Response
           | RedoResponse                                         Redo.Response
           | RemoveConnectionResponse     (ResponseOf RemoveConnection.Request)
           | RemoveNodesResponse               (ResponseOf RemoveNodes.Request)
           | RemovePortResponse                 (ResponseOf RemovePort.Request)
           | RenameNodeResponse                 (ResponseOf RenameNode.Request)
           | RenamePortResponse                 (ResponseOf RenamePort.Request)
           | SearchNodesResponse               (ResponseOf SearchNodes.Request)
           | SetCodeResponse                       (ResponseOf SetCode.Request)
           | SetNodeExpressionResponse   (ResponseOf SetNodeExpression.Request)
           | SetNodesMetaResponse             (ResponseOf SetNodesMeta.Request)
           | SetPortDefaultResponse         (ResponseOf SetPortDefault.Request)
           | SubstituteResponse                 (ResponseOf Substitute.Request)
           | TransactionResponse               (ResponseOf Transaction.Request)
           | TypeCheckResponse                   (ResponseOf TypeCheck.Request)
           | UndoResponse                                         Undo.Response
           deriving (Show, Generic, NFData)

instance EventName Event

instance IsTrackedEvent Event where
    isTracked (UnknownEvent _) = False
    isTracked _                = True
