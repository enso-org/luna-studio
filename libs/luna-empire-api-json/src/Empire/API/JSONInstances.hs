{-# LANGUAGE FlexibleInstances #-}
module Empire.API.JSONInstances where

import           Data.Aeson.Types                       (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Prologue

import           Empire.API.Atom.CloseFile              as CloseFile
import           Empire.API.Atom.GetBuffer              as GetBuffer
import           Empire.API.Atom.IsSaved                as IsSaved
import           Empire.API.Atom.OpenFile               as OpenFile
import           Empire.API.Atom.SaveFile               as SaveFile
import           Empire.API.Atom.SetProject             as SetProject
import           Empire.API.Atom.Substitute             as Substitute
import           Empire.API.Control.EmpireStarted       as EmpireStarted
import           Empire.API.Data.Breadcrumb             as Breadcrumb
import           Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Error                  as Error
import           Empire.API.Data.Graph                  as Graph
import           Empire.API.Data.GraphLocation          as GraphLocation
import           Empire.API.Data.Library                as Library
import           Empire.API.Data.MonadPath              as MonadPath
import           Empire.API.Data.Node                   as Node
import           Empire.API.Data.NodeLoc                as NodeLoc
import           Empire.API.Data.NodeMeta               as NodeMeta
import           Empire.API.Data.Port                   as Port
import           Empire.API.Data.PortDefault            as PortDefault
import           Empire.API.Data.PortRef                as PortRef
import           Empire.API.Data.Position               as Position
import           Empire.API.Data.Project                as Project
import           Empire.API.Data.Size                   as Size
import           Empire.API.Data.TypeRep                as TypeRep
import           Empire.API.Data.ValueType              as ValueType
import           Empire.API.Data.Vector2                as Vector2
import           Empire.API.Graph.AddConnection         as AddConnection
import           Empire.API.Graph.AddNode               as AddNode
import           Empire.API.Graph.AddPort               as AddPort
import           Empire.API.Graph.AddSubgraph           as AddSubgraph
import           Empire.API.Graph.AutolayoutNodes       as AutolayoutNodes
import           Empire.API.Graph.CollaborationUpdate   as CollaborationUpdate
import           Empire.API.Graph.ConnectUpdate         as ConnectUpdate
import           Empire.API.Graph.DumpGraphViz          as DumpGraphViz
import           Empire.API.Graph.GetProgram            as GetProgram
import           Empire.API.Graph.GetSubgraphs          as GetSubgraphs
import           Empire.API.Graph.MonadsUpdate          as MonadsUpdate
import           Empire.API.Graph.MovePort              as MovePort
import           Empire.API.Graph.NodeResultUpdate      as NodeResultUpdate
import           Empire.API.Graph.NodesUpdate           as NodesUpdate
import           Empire.API.Graph.NodeTypecheckerUpdate as NodeTypecheckerUpdate
import           Empire.API.Graph.Redo                  as Redo
import           Empire.API.Graph.RemoveConnection      as RemoveConnection
import           Empire.API.Graph.RemoveNodes           as RemoveNodes
import           Empire.API.Graph.RemovePort            as RemovePort
import           Empire.API.Graph.RenameNode            as RenameNode
import           Empire.API.Graph.RenamePort            as RenamePort
import           Empire.API.Graph.SearchNodes           as SearchNodes
import           Empire.API.Graph.SetNodeCode           as SetNodeCode
import           Empire.API.Graph.SetNodeExpression     as SetNodeExpression
import           Empire.API.Graph.SetNodesMeta          as SetNodesMeta
import           Empire.API.Graph.SetPortDefault        as SetPortDefault
import           Empire.API.Graph.TypeCheck             as TypeCheck
import           Empire.API.Graph.Undo                  as Undo
import           Empire.API.Library.CreateLibrary       as CreateLibrary
import           Empire.API.Library.ListLibraries       as ListLibraries
import           Empire.API.Persistence.Envelope        as PEnvelope
import           Empire.API.Persistence.Library         as PLibrary
import           Empire.API.Persistence.Project         as PProject
import           Empire.API.Project.CreateProject       as CreateProject
import           Empire.API.Project.ExportProject       as ExportProject
import           Empire.API.Project.ImportProject       as ImportProject
import           Empire.API.Project.ListProjects        as ListProjects
import           Empire.API.Project.OpenProject         as OpenProject
import           Empire.API.Request                     as Request
import           Empire.API.Response                    as Response


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


instance ToJSON AddConnection.Request
instance ToJSON AddConnection.Result

instance ToJSON AddNode.Request
instance ToJSON AddNode.Result

instance ToJSON AddPort.Request
instance ToJSON AddPort.Result

instance ToJSON AddSubgraph.Request
instance ToJSON AddSubgraph.Result

instance ToJSON AutolayoutNodes.Request
instance ToJSON AutolayoutNodes.Inverse

instance ToJSON CollaborationUpdate.Update
instance ToJSON CollaborationUpdate.Event

instance ToJSON ConnectUpdate.Update

instance ToJSON DumpGraphViz.Request

instance ToJSON GetProgram.Request
instance ToJSON GetProgram.Result

instance ToJSON GetSubgraphs.Request
instance ToJSON GetSubgraphs.Result

instance ToJSON MonadsUpdate.Update

instance ToJSON MovePort.Request

instance ToJSON NodesUpdate.Update

instance ToJSON NodeResultUpdate.Update
instance ToJSON NodeResultUpdate.NodeValue

instance ToJSON NodeTypecheckerUpdate.Update

instance ToJSON Redo.RedoRequest
instance ToJSON Redo.Request



instance ToJSON GetBuffer.Request
instance ToJSON GetBuffer.Result

instance ToJSON Substitute.Request
instance ToJSON Substitute.Update

instance ToJSON RemoveConnection.Request
instance ToJSON RemoveConnection.Inverse
instance ToJSON RemoveConnection.Update

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

instance ToJSON SetNodeCode.Request
instance ToJSON SetNodeCode.Inverse

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
