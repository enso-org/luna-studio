module Reactive.State.ConnectionPen where


import           Data.Aeson               (ToJSON)
import           Empire.API.Data.Node     (NodeId)
import           Empire.API.JSONInstances ()
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

data DrawingType = Connecting | Disconnecting deriving (Show, Eq, Generic)

data Drawing = Drawing { _previousPos  :: Vector2 Int
                       , _drawingType  :: DrawingType
                       , _lastNode     :: Maybe NodeId
                       , _visitedNodes :: [NodeId]
                       } deriving (Eq, Show, Generic)


data State = State { _drawing :: Maybe Drawing
                   } deriving (Eq, Show, Generic)


makeLenses ''State
makeLenses ''Drawing

instance ToJSON DrawingType
instance ToJSON Drawing
instance ToJSON State

instance Default State where
    def = State def
