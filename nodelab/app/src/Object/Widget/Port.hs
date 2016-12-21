module Object.Widget.Port where

import           Data.Aeson               (ToJSON)
import           Luna.Studio.Prelude      hiding (set)

import qualified Empire.API.Data.Node     as API
import qualified Empire.API.Data.Port     as API
import           Empire.API.Data.PortRef  (AnyPortRef, toAnyPortRef)
import qualified Empire.API.JSONInstances ()
import           Luna.Studio.Data.Color   (Color)
import           Luna.Studio.Data.Color   (colorPort)



data Port = Port { _portRef     :: AnyPortRef
                 -- TODO[react]: Find out if we need portId here since it seems to be present in portRef
                 , _portId      :: API.PortId
                 , _color       :: Color
                 } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Port
instance ToJSON Port

fromPorts :: API.NodeId -> [API.Port] -> [Port]
fromPorts nodeId ports = fromPort nodeId <$> ports where

fromPort :: API.NodeId -> API.Port -> Port
fromPort nodeId port = Port portRef' portId' (colorPort port) where
    portId'  = port ^. API.portId
    portRef' = toAnyPortRef nodeId portId'

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
