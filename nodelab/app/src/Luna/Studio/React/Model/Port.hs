{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Model.Port where

import           Data.Aeson              (ToJSON)
import           Data.Position           (Position)

import           Empire.API.Data.Node    (NodeId)
import qualified Empire.API.Data.Port    as API
import           Empire.API.Data.PortRef (AnyPortRef, toAnyPortRef)
import           Empire.API.Data.TypeRep (TypeRep)
import           Luna.Studio.Data.Color  (Color)
import qualified Luna.Studio.Data.Color  as Color
import           Luna.Studio.Prelude     hiding (set)



data Port = Port { _portRef     :: AnyPortRef
                 , _port        :: API.Port
                 , _color       :: Color
                 , _highlight   :: Bool
                 , _visible     :: Bool
                 } deriving (Eq, Show, Typeable, Generic, NFData)

makeLenses ''Port
instance ToJSON Port

data DraggedPort = DraggedPort { _draggedPort       :: Port
                               , _positionInSidebar :: Position
                               } deriving (Eq, Show, Typeable, Generic, NFData)

makeLenses ''DraggedPort
instance ToJSON DraggedPort

portId :: Lens' Port API.PortId
portId = port . API.portId

name :: Lens' Port String
name = port . API.name

valueType :: Lens' Port TypeRep
valueType = port . API.valueType

state :: Lens' Port API.PortState
state = port . API.state

fromPorts :: NodeId -> [API.Port] -> [Port]
fromPorts nodeId ports = fromPort nodeId <$> ports

fromPort :: NodeId -> API.Port -> Port
fromPort nodeId p = Port portRef' p (Color.fromPort p) False True where
    portRef' = toAnyPortRef nodeId $ p ^. API.portId
