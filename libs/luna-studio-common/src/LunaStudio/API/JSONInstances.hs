{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LunaStudio.API.JSONInstances where

import           Data.Aeson.Types                           (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           LunaStudio.API.Atom.CloseFile              as CloseFile
import           LunaStudio.API.Atom.GetBuffer              as GetBuffer
import           LunaStudio.API.Atom.IsSaved                as IsSaved
import           LunaStudio.API.Atom.Lexer                  as Lexer
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
import           LunaStudio.API.Graph.DumpGraphViz          as DumpGraphViz
import           LunaStudio.API.Graph.GetProgram            as GetProgram
import           LunaStudio.API.Graph.GetSubgraphs          as GetSubgraphs
import           LunaStudio.API.Graph.MonadsUpdate          as MonadsUpdate
import           LunaStudio.API.Graph.MovePort              as MovePort
import           LunaStudio.API.Graph.NodeResultUpdate      as NodeResultUpdate
import           LunaStudio.API.Graph.NodeTypecheckerUpdate as NodeTypecheckerUpdate
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
import           LunaStudio.Data.Breadcrumb                 as Breadcrumb
import           LunaStudio.Data.Connection                 as Connection
import           LunaStudio.Data.Error                      as Error
import           LunaStudio.Data.Graph                      as Graph
import           LunaStudio.Data.GraphLocation              as GraphLocation
import           LunaStudio.Data.Library                    as Library
import           LunaStudio.Data.MonadPath                  as MonadPath
import           LunaStudio.Data.Node                       as Node
import           LunaStudio.Data.NodeLoc                    as NodeLoc
import           LunaStudio.Data.NodeMeta                   as NodeMeta
import           LunaStudio.Data.Port                       as Port
import           LunaStudio.Data.PortDefault                as PortDefault
import           LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.Position                   as Position
import           LunaStudio.Data.Project                    as Project
import           LunaStudio.Data.Size                       as Size
import           LunaStudio.Data.TypeRep                    as TypeRep
import           LunaStudio.Data.ValueType                  as ValueType
import           LunaStudio.Data.Vector2                    as Vector2


instance FromJSON a => FromJSON (Vector2.Vector2 a)
instance ToJSON   a => ToJSON (Vector2.Vector2 a)

instance FromJSON  Position.Position
instance ToJSON    Position.Position

instance ToJSON    Size.Size


instance ToJSON Project.Project
instance ToJSON Library.Library

instance ToJSON a   => ToJSON (Breadcrumb.Breadcrumb a)
instance ToJSON a   => ToJSON (Breadcrumb.Named a)
instance               ToJSON Breadcrumb.BreadcrumbItem

instance FromJSON a => FromJSON (Breadcrumb.Breadcrumb a)
instance FromJSON a => FromJSON (Breadcrumb.Named a)
instance               FromJSON Breadcrumb.BreadcrumbItem

instance ToJSON GraphLocation.GraphLocation
instance FromJSON GraphLocation.GraphLocation

instance ToJSON   Node.ExpressionNode
instance FromJSON Node.ExpressionNode
instance ToJSON   Node.InputSidebar
instance FromJSON Node.InputSidebar
instance ToJSON   Node.OutputSidebar
instance FromJSON Node.OutputSidebar
instance ToJSON   Node.Node
instance FromJSON Node.Node
instance ToJSON   Node.NodeTypecheckerUpdate
instance FromJSON Node.NodeTypecheckerUpdate

instance ToJSON NodeMeta.NodeMeta
instance FromJSON NodeMeta.NodeMeta

instance ToJSON NodeLoc
instance ToJSON NodePath
instance FromJSON NodeLoc
instance FromJSON NodePath

instance FromJSONKey AnyPortRef
instance FromJSONKey Breadcrumb.BreadcrumbItem
instance FromJSONKey InPortRef
instance FromJSONKey NodeLoc
instance ToJSONKey AnyPortRef
instance ToJSONKey Breadcrumb.BreadcrumbItem
instance ToJSONKey InPortRef
instance ToJSONKey NodeLoc

instance ToJSON i => ToJSON (Port.Port i)
instance FromJSON i => FromJSON (Port.Port i)
instance ToJSON Port.AnyPortId
instance FromJSON Port.AnyPortId
instance ToJSON Port.PortState
instance FromJSON Port.PortState

instance ToJSON TypeRep.TypeRep
instance FromJSON TypeRep.TypeRep

instance ToJSON ValueType.ValueTypeEnum

instance ToJSON PortRef.AnyPortRef
instance FromJSON PortRef.AnyPortRef
instance ToJSON PortRef.OutPortRef
instance FromJSON PortRef.OutPortRef
instance ToJSON PortRef.InPortRef
instance FromJSON PortRef.InPortRef

instance ToJSON Connection.Connection
instance FromJSON Connection.Connection

instance ToJSON PortDefault.VisualizationValue
instance FromJSON PortDefault.VisualizationValue
instance ToJSON PortDefault.PortValue
instance FromJSON PortDefault.PortValue
instance ToJSON PortDefault.PortDefault
instance FromJSON PortDefault.PortDefault

instance ToJSON Graph.Graph
instance FromJSON Graph.Graph

instance ToJSON MonadPath.MonadPath
instance FromJSON MonadPath.MonadPath

instance ToJSON IsSaved.Saved

instance ToJSON Error.ErrorType
instance ToJSON Error.Error

instance ToJSON Result.Result
instance FromJSON Result.Result


instance ToJSON AddConnection.Request
instance ToJSON AddConnection.Inverse

instance ToJSON AddNode.Request

instance ToJSON AddPort.Request

instance ToJSON AddSubgraph.Request

instance ToJSON AutolayoutNodes.Request
instance ToJSON AutolayoutNodes.Inverse

instance ToJSON CollaborationUpdate.Update
instance ToJSON CollaborationUpdate.Event

instance ToJSON DumpGraphViz.Request

instance ToJSON GetProgram.Request
instance ToJSON GetProgram.Result

instance ToJSON GetSubgraphs.Request
instance ToJSON GetSubgraphs.Result

instance ToJSON MonadsUpdate.Update

instance ToJSON MovePort.Request

instance ToJSON NodeResultUpdate.Update
instance ToJSON NodeResultUpdate.NodeValue

instance ToJSON NodeTypecheckerUpdate.Update

instance ToJSON Redo.RedoRequest
instance ToJSON Redo.Request



instance ToJSON GetBuffer.Request
instance ToJSON GetBuffer.Result

instance ToJSON Lexer.Update

instance ToJSON Substitute.Request
instance ToJSON Substitute.Update

instance ToJSON RemoveConnection.Request
instance ToJSON RemoveConnection.Inverse

instance ToJSON RemoveNodes.Request
instance ToJSON RemoveNodes.Inverse

instance ToJSON RemovePort.Request
instance ToJSON RemovePort.Inverse

instance ToJSON RenameNode.Request
instance ToJSON RenameNode.Inverse

instance ToJSON RenamePort.Request
instance ToJSON RenamePort.Inverse

instance ToJSON a => ToJSON (Request.Request a)

instance ToJSON SearchNodes.Request
instance ToJSON SearchNodes.Result

instance ToJSON SetNodeExpression.Request
instance ToJSON SetNodeExpression.Inverse

instance ToJSON SetNodesMeta.Request
instance ToJSON SetNodesMeta.Inverse

instance ToJSON SetPortDefault.Request
instance ToJSON SetPortDefault.Inverse

instance ToJSON TypeCheck.Request

instance ToJSON Undo.UndoRequest
instance ToJSON Undo.Request


instance ToJSON CreateLibrary.Request
instance ToJSON CreateLibrary.Result
instance ToJSON CreateLibrary.Update

instance ToJSON ListLibraries.Request
instance ToJSON ListLibraries.Result

instance ToJSON CreateProject.Request
instance ToJSON CreateProject.Result
instance ToJSON CreateProject.Update

instance ToJSON OpenProject.Request
instance ToJSON OpenProject.Result
instance ToJSON OpenProject.Update

instance ToJSON ListProjects.Request
instance ToJSON ListProjects.Result
instance ToJSON ListProjects.Update

instance ToJSON ExportProject.Request
instance ToJSON ExportProject.Result

instance ToJSON ImportProject.Request
instance ToJSON ImportProject.Result

instance ToJSON CloseFile.Request

instance ToJSON OpenFile.Request

instance ToJSON SaveFile.Request

instance ToJSON IsSaved.Request
instance ToJSON IsSaved.Result

instance ToJSON SetProject.Request


instance (ToJSON req, ToJSON res, ToJSON inv) => ToJSON (Response.Response req inv res)
instance ToJSON payload => ToJSON (Response.Status payload)

instance ToJSON EmpireStarted.Status

instance ToJSON PProject.Project
instance FromJSON PProject.Project
instance ToJSON PLibrary.Library
instance FromJSON PLibrary.Library
instance ToJSON PEnvelope.Envelope
instance FromJSON PEnvelope.Envelope
