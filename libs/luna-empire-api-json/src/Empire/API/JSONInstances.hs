{-# LANGUAGE FlexibleInstances #-}
module Empire.API.JSONInstances where

import           Data.Aeson.Types                      (FromJSON, ToJSON, parseJSON, toJSON, typeMismatch)
import qualified Data.Aeson.Types                      as JSONTypes
import           Data.Map.Lazy                         (Map)
import qualified Data.Map.Lazy                         as Map
import           Data.Maybe                            (fromMaybe)
import qualified Data.Text                             as Text
import           Data.UUID.Types                       (UUID)
import qualified Data.UUID.Types                       as UUID
import           Prologue
import           Text.Read                             (readMaybe)

import           Empire.API.Control.EmpireStarted      as EmpireStarted
import           Empire.API.Data.Breadcrumb            as Breadcrumb
import           Empire.API.Data.Connection            as Connection
import           Empire.API.Data.DefaultValue          as DefaultValue
import           Empire.API.Data.Error                 as Error
import           Empire.API.Data.Graph                 as Graph
import           Empire.API.Data.GraphLocation         as GraphLocation
import           Empire.API.Data.Input                 as Input
import           Empire.API.Data.Library               as Library
import           Empire.API.Data.Node                  as Node
import           Empire.API.Data.NodeMeta              as NodeMeta
import           Empire.API.Data.Output                as Output
import           Empire.API.Data.Port                  as Port
import           Empire.API.Data.PortRef               as PortRef
import           Empire.API.Data.Project               as Project
import           Empire.API.Data.TypeRep               as TypeRep
import           Empire.API.Data.ValueType             as ValueType
import           Empire.API.Graph.AddNode              as AddNode
import           Empire.API.Graph.AddSubgraph          as AddSubgraph
import           Empire.API.Graph.CodeUpdate           as CodeUpdate
import           Empire.API.Graph.Collaboration        as Collaboration
import           Empire.API.Graph.Connect              as Connect
import           Empire.API.Graph.Disconnect           as Disconnect
import           Empire.API.Graph.GetProgram           as GetProgram
import           Empire.API.Graph.NodeResultUpdate     as NodeResultUpdate
import           Empire.API.Graph.NodeSearcherUpdate   as NodeSearcherUpdate
import           Empire.API.Graph.NodesUpdate          as NodesUpdate
import           Empire.API.Graph.RemoveNodes          as RemoveNodes
import           Empire.API.Graph.RenameNode           as RenameNode
import           Empire.API.Graph.SetDefaultValue      as SetDefaultValue
import           Empire.API.Graph.SetInputNodeType     as SetInputNodeType
import           Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
import           Empire.API.Graph.UpdateNodeMeta       as UpdateNodeMeta
import           Empire.API.Library.CreateLibrary      as CreateLibrary
import           Empire.API.Library.ListLibraries      as ListLibraries
import           Empire.API.Persistence.Envelope       as PEnvelope
import           Empire.API.Persistence.Library        as PLibrary
import           Empire.API.Persistence.Project        as PProject
import           Empire.API.Project.CreateProject      as CreateProject
import           Empire.API.Project.ExportProject      as ExportProject
import           Empire.API.Project.ImportProject      as ImportProject
import           Empire.API.Project.ListProjects       as ListProjects
import           Empire.API.Request                    as Request
import           Empire.API.Response                   as Response


instance ToJSON Project.Project
instance ToJSON Library.Library

instance ToJSON a   => ToJSON (Breadcrumb.Breadcrumb a)
instance ToJSON a   => ToJSON (Breadcrumb.Named a)
instance               ToJSON Breadcrumb.BreadcrumbItem

instance FromJSON a => FromJSON (Breadcrumb.Breadcrumb a)
instance FromJSON a => FromJSON (Breadcrumb.Named a)
instance               FromJSON Breadcrumb.BreadcrumbItem

instance ToJSON GraphLocation.GraphLocation

instance ToJSON Node.Node
instance FromJSON Node.Node
instance ToJSON Node.NodeType
instance FromJSON Node.NodeType

instance ToJSON NodeMeta.NodeMeta
instance FromJSON NodeMeta.NodeMeta

instance {-# OVERLAPPING #-} (ToJSON b) => ToJSON (Map UUID b) where
    toJSON = toJSON . Map.mapKeys UUID.toString
    {-# INLINE toJSON #-}

instance (ToJSON b) => ToJSON  (Map AnyPortRef b) where
    toJSON = toJSON . Map.mapKeys show
    {-# INLINE toJSON #-}

instance (ToJSON b) => ToJSON  (Map InPortRef b) where
    toJSON = toJSON . Map.mapKeys show
    {-# INLINE toJSON #-}

instance (ToJSON b) => ToJSON  (Map OutPortRef b) where
    toJSON = toJSON . Map.mapKeys show
    {-# INLINE toJSON #-}

instance {-# OVERLAPPING #-} (ToJSON b) => ToJSON  (Map PortId b) where
    toJSON = toJSON . Map.mapKeys show
    {-# INLINE toJSON #-}

instance {-# OVERLAPPING #-} (FromJSON b) => FromJSON  (Map PortId b) where
    parseJSON = fmap (Map.mapKeys read) . parseJSON -- TODO: use readMaybe
    {-# INLINE parseJSON #-}

instance ToJSON Port.Port
instance FromJSON Port.Port
instance ToJSON Port.InPort
instance FromJSON Port.InPort
instance ToJSON Port.OutPort
instance FromJSON Port.OutPort
instance ToJSON Port.PortId
instance FromJSON Port.PortId
instance ToJSON Port.PortState
instance FromJSON Port.PortState

instance ToJSON TypeRep.TypeRep
instance FromJSON TypeRep.TypeRep

instance ToJSON ValueType.ValueType
instance FromJSON ValueType.ValueType
instance ToJSON ValueType.ValueTypeEnum

instance ToJSON PortRef.AnyPortRef
instance FromJSON PortRef.AnyPortRef
instance ToJSON PortRef.OutPortRef
instance FromJSON PortRef.OutPortRef
instance ToJSON PortRef.InPortRef
instance FromJSON PortRef.InPortRef

instance ToJSON Connection.Connection
instance FromJSON Connection.Connection

instance ToJSON DefaultValue.Value
instance FromJSON DefaultValue.Value
instance ToJSON DefaultValue.PortDefault
instance FromJSON DefaultValue.PortDefault

instance ToJSON Graph.Graph
instance FromJSON Graph.Graph
instance ToJSON Input.Input
instance FromJSON Input.Input
instance ToJSON Output.Output
instance FromJSON Output.Output

instance ToJSON t => ToJSON (Error.Error t)

instance ToJSON AddNode.NodeType
instance ToJSON AddNode.Request
instance ToJSON AddNode.Update

instance ToJSON AddSubgraph.Request

instance ToJSON Connect.Request
instance ToJSON Connect.Update

instance ToJSON Disconnect.Request
instance ToJSON Disconnect.Update

instance ToJSON RemoveNodes.Request
instance ToJSON RemoveNodes.Update

instance ToJSON RenameNode.Request
instance ToJSON RenameNode.Update

instance ToJSON UpdateNodeMeta.Request
instance ToJSON UpdateNodeMeta.Update

instance ToJSON NodesUpdate.Update

instance ToJSON UpdateNodeExpression.Request

instance ToJSON NodeResultUpdate.Update
instance ToJSON NodeResultUpdate.NodeValue

instance ToJSON CodeUpdate.Update

instance ToJSON GetProgram.Request
instance ToJSON GetProgram.Result

instance ToJSON SetDefaultValue.Request

instance ToJSON SetInputNodeType.Request

instance ToJSON Collaboration.Update
instance ToJSON Collaboration.Event

instance ToJSON CreateLibrary.Request
instance ToJSON CreateLibrary.Result
instance ToJSON CreateLibrary.Update

instance ToJSON ListLibraries.Request
instance ToJSON ListLibraries.Result

instance ToJSON CreateProject.Request
instance ToJSON CreateProject.Result
instance ToJSON CreateProject.Update

instance ToJSON ListProjects.Request
instance ToJSON ListProjects.Result
instance ToJSON ListProjects.Update

instance ToJSON ExportProject.Request
instance ToJSON ExportProject.Result

instance ToJSON ImportProject.Request
instance ToJSON ImportProject.Result


instance (ToJSON req, ToJSON res) => ToJSON (Response.Response req res)
instance (ToJSON payload) => ToJSON (Response.Status payload)

instance ToJSON NodeSearcherUpdate.Update

instance ToJSON EmpireStarted.Status

instance ToJSON PProject.Project
instance FromJSON PProject.Project
instance ToJSON PLibrary.Library
instance FromJSON PLibrary.Library
instance ToJSON PEnvelope.Envelope
instance FromJSON PEnvelope.Envelope

instance (ToJSON a) => ToJSON (Request.Request a)
instance ToJSON UUID where
  toJSON = toJSON . UUID.toString
instance FromJSON UUID where
  parseJSON (JSONTypes.String w) = case (UUID.fromString $ Text.unpack w) of
    Just s  -> return s
    Nothing -> fail "expected UUID"
  parseJSON w = typeMismatch "UUID" w
