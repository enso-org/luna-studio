{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LunaStudio.API.JSONInstances where

import           Data.Aeson.Types                           (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           LunaStudio.API.Atom.CloseFile              as CloseFile
import           LunaStudio.API.Atom.FileChanged            as FileChanged
import           LunaStudio.API.Atom.GetBuffer              as GetBuffer
import           LunaStudio.API.Atom.IsSaved                as IsSaved
import           LunaStudio.API.Atom.OpenFile               as OpenFile
import           LunaStudio.API.Atom.SaveFile               as SaveFile
import           LunaStudio.API.Atom.SetProject             as SetProject
import           LunaStudio.API.Atom.Substitute             as Substitute
import           LunaStudio.API.Control.EmpireStarted       as EmpireStarted
import           LunaStudio.API.Graph.AddConnection         as AddConnection
import           LunaStudio.API.Graph.AddNode               as AddNode
import           LunaStudio.API.Graph.AddPort               as AddPort
import           LunaStudio.API.Graph.AddSubgraph           as AddSubgraph
import           LunaStudio.API.Graph.AutolayoutNodes       as AutolayoutNodes
import           LunaStudio.API.Graph.CollaborationUpdate   as CollaborationUpdate
import           LunaStudio.API.Graph.CollapseToFunction    as CollapseToFunction
import           LunaStudio.API.Graph.Copy                  as Copy
import           LunaStudio.API.Graph.DumpGraphViz          as DumpGraphViz
import           LunaStudio.API.Graph.GetProgram            as GetProgram
import           LunaStudio.API.Graph.GetSubgraphs          as GetSubgraphs
import           LunaStudio.API.Graph.MonadsUpdate          as MonadsUpdate
import           LunaStudio.API.Graph.MovePort              as MovePort
import           LunaStudio.API.Graph.NodeResultUpdate      as NodeResultUpdate
import           LunaStudio.API.Graph.NodeTypecheckerUpdate as NodeTypecheckerUpdate
import           LunaStudio.API.Graph.Paste                 as Paste
import           LunaStudio.API.Graph.Redo                  as Redo
import           LunaStudio.API.Graph.RemoveConnection      as RemoveConnection
import           LunaStudio.API.Graph.RemoveNodes           as RemoveNodes
import           LunaStudio.API.Graph.RemovePort            as RemovePort
import           LunaStudio.API.Graph.RenameNode            as RenameNode
import           LunaStudio.API.Graph.RenamePort            as RenamePort
import           LunaStudio.API.Graph.Result                as Result
import           LunaStudio.API.Graph.SearchNodes           as SearchNodes
import           LunaStudio.API.Graph.SetNodeExpression     as SetNodeExpression
import           LunaStudio.API.Graph.SetNodesMeta          as SetNodesMeta
import           LunaStudio.API.Graph.SetPortDefault        as SetPortDefault
import           LunaStudio.API.Graph.TypeCheck             as TypeCheck
import           LunaStudio.API.Graph.Undo                  as Undo
import           LunaStudio.API.Library.CreateLibrary       as CreateLibrary
import           LunaStudio.API.Library.ListLibraries       as ListLibraries
import           LunaStudio.API.Persistence.Envelope        as PEnvelope
import           LunaStudio.API.Persistence.Library         as PLibrary
import           LunaStudio.API.Persistence.Project         as PProject
import           LunaStudio.API.Project.CreateProject       as CreateProject
import           LunaStudio.API.Project.ExportProject       as ExportProject
import           LunaStudio.API.Project.ImportProject       as ImportProject
import           LunaStudio.API.Project.ListProjects        as ListProjects
import           LunaStudio.API.Project.OpenProject         as OpenProject
import           LunaStudio.API.Request                     as Request
import           LunaStudio.API.Response                    as Response
import           LunaStudio.Data.Connection                 as Connection
import           LunaStudio.Data.Error                      as Error
import           LunaStudio.Data.Graph                      as Graph
import           LunaStudio.Data.Library                    as Library
import           LunaStudio.Data.MonadPath                  as MonadPath
import           LunaStudio.Data.Node                       as Node
import           LunaStudio.Data.NodeLoc                    as NodeLoc
import           LunaStudio.Data.NodeMeta                   as NodeMeta
import           LunaStudio.Data.NodeValue                  as NodeValue
import           LunaStudio.Data.Point                      as Point
import           LunaStudio.Data.Port                       as Port
import           LunaStudio.Data.PortDefault                as PortDefault
import           LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.Position                   as Position
import           LunaStudio.Data.Project                    as Project
import           LunaStudio.Data.Size                       as Size
import           LunaStudio.Data.ValueType                  as ValueType
import           LunaStudio.Data.Vector2                    as Vector2


instance FromJSON a => FromJSON (Vector2.Vector2 a)
instance FromJSON Connection.Connection
instance FromJSON Graph.Graph
instance FromJSON i => FromJSON (Port.Port i)
instance FromJSON MonadPath.MonadPath
instance FromJSON Node.ExpressionNode
instance FromJSON Node.InputSidebar
instance FromJSON Node.Node
instance FromJSON Node.NodeTypecheckerUpdate
instance FromJSON Node.OutputSidebar
instance FromJSON NodeLoc
instance FromJSON NodeMeta.NodeMeta
instance FromJSON NodePath
instance FromJSON NodeValue.VisualizationValue
instance FromJSON PEnvelope.Envelope
instance FromJSON PLibrary.Library
instance FromJSON Point
instance FromJSON Port.AnyPortId
instance FromJSON Port.PortState
instance FromJSON PortDefault.PortDefault
instance FromJSON PortDefault.PortValue
instance FromJSON PortRef.AnyPortRef
instance FromJSON PortRef.InPortRef
instance FromJSON PortRef.OutPortRef
instance FromJSON Position.Position
instance FromJSON PProject.Project
instance FromJSON Result.Result

instance FromJSONKey AnyPortRef
instance FromJSONKey InPortRef
instance FromJSONKey NodeLoc

instance (ToJSON req, ToJSON res, ToJSON inv) => ToJSON (Response.Response req inv res)
instance ToJSON a => ToJSON (Request.Request a)
instance ToJSON a => ToJSON (Vector2.Vector2 a)
instance ToJSON AddConnection.Inverse
instance ToJSON AddConnection.Request
instance ToJSON AddNode.Request
instance ToJSON AddPort.Request
instance ToJSON AddSubgraph.Request
instance ToJSON AutolayoutNodes.Inverse
instance ToJSON AutolayoutNodes.Request
instance ToJSON CloseFile.Request
instance ToJSON CollaborationUpdate.Event
instance ToJSON CollaborationUpdate.Update
instance ToJSON CollapseToFunction.Inverse
instance ToJSON CollapseToFunction.Request
instance ToJSON Connection.Connection
instance ToJSON Copy.Request
instance ToJSON Copy.Result
instance ToJSON CreateLibrary.Request
instance ToJSON CreateLibrary.Result
instance ToJSON CreateLibrary.Update
instance ToJSON CreateProject.Request
instance ToJSON CreateProject.Result
instance ToJSON CreateProject.Update
instance ToJSON DumpGraphViz.Request
instance ToJSON EmpireStarted.Status
instance ToJSON Error.Error
instance ToJSON Error.ErrorType
instance ToJSON ExportProject.Request
instance ToJSON ExportProject.Result
instance ToJSON FileChanged.Request
instance ToJSON GetBuffer.Request
instance ToJSON GetBuffer.Result
instance ToJSON GetProgram.Request
instance ToJSON GetProgram.Result
instance ToJSON GetSubgraphs.Request
instance ToJSON GetSubgraphs.Result
instance ToJSON Graph.Graph
instance ToJSON i => ToJSON (Port.Port i)
instance ToJSON ImportProject.Request
instance ToJSON ImportProject.Result
instance ToJSON IsSaved.Request
instance ToJSON IsSaved.Result
instance ToJSON IsSaved.Saved
instance ToJSON Library.Library
instance ToJSON ListLibraries.Request
instance ToJSON ListLibraries.Result
instance ToJSON ListProjects.Request
instance ToJSON ListProjects.Result
instance ToJSON ListProjects.Update
instance ToJSON MonadPath.MonadPath
instance ToJSON MonadsUpdate.Update
instance ToJSON MovePort.Request
instance ToJSON Node.ExpressionNode
instance ToJSON Node.InputSidebar
instance ToJSON Node.Node
instance ToJSON Node.NodeTypecheckerUpdate
instance ToJSON Node.OutputSidebar
instance ToJSON NodeLoc
instance ToJSON NodeMeta.NodeMeta
instance ToJSON NodePath
instance ToJSON NodeResultUpdate.Update
instance ToJSON NodeTypecheckerUpdate.Update
instance ToJSON NodeValue.NodeValue
instance ToJSON NodeValue.VisualizationValue
instance ToJSON OpenFile.Request
instance ToJSON OpenProject.Request
instance ToJSON OpenProject.Result
instance ToJSON OpenProject.Update
instance ToJSON payload => ToJSON (Response.Status payload)
instance ToJSON Paste.Request
instance ToJSON PEnvelope.Envelope
instance ToJSON PLibrary.Library
instance ToJSON Point
instance ToJSON Port.AnyPortId
instance ToJSON Port.PortState
instance ToJSON PortDefault.PortDefault
instance ToJSON PortDefault.PortValue
instance ToJSON PortRef.AnyPortRef
instance ToJSON PortRef.InPortRef
instance ToJSON PortRef.OutPortRef
instance ToJSON Position.Position
instance ToJSON PProject.Project
instance ToJSON Project.Project
instance ToJSON Redo.RedoRequest
instance ToJSON Redo.Request
instance ToJSON RemoveConnection.Inverse
instance ToJSON RemoveConnection.Request
instance ToJSON RemoveNodes.Inverse
instance ToJSON RemoveNodes.Request
instance ToJSON RemovePort.Inverse
instance ToJSON RemovePort.Request
instance ToJSON RenameNode.Inverse
instance ToJSON RenameNode.Request
instance ToJSON RenamePort.Inverse
instance ToJSON RenamePort.Request
instance ToJSON Result.Result
instance ToJSON SaveFile.Request
instance ToJSON SearchNodes.Request
instance ToJSON SearchNodes.Result
instance ToJSON SetNodeExpression.Inverse
instance ToJSON SetNodeExpression.Request
instance ToJSON SetNodesMeta.Inverse
instance ToJSON SetNodesMeta.Request
instance ToJSON SetPortDefault.Inverse
instance ToJSON SetPortDefault.Request
instance ToJSON SetProject.Request
instance ToJSON Size.Size
instance ToJSON Substitute.Request
instance ToJSON Substitute.Update
instance ToJSON TypeCheck.Request
instance ToJSON Undo.Request
instance ToJSON Undo.UndoRequest
instance ToJSON ValueType.ValueTypeEnum

instance ToJSONKey AnyPortRef
instance ToJSONKey InPortRef
instance ToJSONKey NodeLoc
