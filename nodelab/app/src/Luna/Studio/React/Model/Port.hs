{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Model.Port where

import           Data.Aeson                (ToJSON)

import           Empire.API.Data.Node      (NodeId)
import qualified Empire.API.Data.Port      as API
import           Empire.API.Data.PortRef   (AnyPortRef, toAnyPortRef)
import           Empire.API.Data.ValueType (ValueType)
import           Luna.Studio.Data.Color    (Color)
import           Luna.Studio.Data.Color    (colorPort)
import           Luna.Studio.Prelude       hiding (set)



data Port = Port { _portRef     :: AnyPortRef
                 , _port        :: API.Port
                 , _color       :: Color
                 } deriving (Eq, Show, Typeable, Generic, NFData)

makeLenses ''Port
instance ToJSON Port

portId :: Lens' Port API.PortId
portId = port . API.portId

name :: Lens' Port String
name = port . API.name

valueType :: Lens' Port ValueType
valueType = port . API.valueType

state :: Lens' Port API.PortState
state = port . API.state

fromPorts :: NodeId -> [API.Port] -> [Port]
fromPorts nodeId ports = fromPort nodeId <$> ports where

fromPort :: NodeId -> API.Port -> Port
fromPort nodeId port = Port portRef' port (colorPort port) where
    portRef' = toAnyPortRef nodeId $ port ^. API.portId

-- TODO[react]: Should be removed
-- angle :: Getter Port Double
-- angle = to (toAngle . view angleVector )
--
--
-- angleToDimVec :: Double -> Vector2 Double
-- angleToDimVec angle' = (/ 10.0) <$> Vector2 (cos angle') (-sin angle')
--
-- angleToDimVec' :: Double -> Vector2 Double
-- angleToDimVec' angle' = (/ 10.0) <$> Vector2 (-cos angle') (-sin angle')
--
-- defaultAngle :: Int -> PortId -> Vector2 Double
-- defaultAngle numPorts (OutPortId All)                  = angleToDimVec' pi where
-- defaultAngle numPorts (OutPortId (Projection portNum)) = angleToDimVec' angle' where
--     angle' = delta * (fromIntegral portNum) + delta / 2.0 + pi / 2.0
--     delta  = pi / (fromIntegral numPorts)
-- defaultAngle _        (InPortId (Self))                = angleToDimVec 0.0
-- defaultAngle numPorts (InPortId (Arg portNum))         = angleToDimVec angle' where
--     angle' = delta * (fromIntegral portNum) + delta / 2.0 + pi / 2.0
--     delta  = pi / (fromIntegral numPorts)
