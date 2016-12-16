module Object.Widget.Port where

import           Data.Aeson               (ToJSON)
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude      hiding (set)

import           Empire.API.Data.Port     (PortId (..))
import           Empire.API.Data.PortRef  (AnyPortRef)
import qualified Empire.API.JSONInstances ()
import           Luna.Studio.Data.Color   (Color)

import           Object.Widget

data Port = Port { _portRef     :: AnyPortRef
                 -- TODO[react]: Find out if we need portId here since it seems to be present in portRef
                 , _portId      :: PortId
                 , _color       :: Color
                 , _highlight   :: Bool
                 } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Port
instance ToJSON Port

instance IsDisplayObject Port where
    widgetPosition = lens (\_ -> Vector2 0.0 0.0) (error "Port has no position setter")
    widgetSize     = lens get set where
        get _      = Vector2 0.0 0.0
        set w _    = w
    widgetVisible  = to $ const True

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
