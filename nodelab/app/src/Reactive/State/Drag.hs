module Reactive.State.Drag where

import           Data.Aeson               (ToJSON)
import           Data.Map                 (Map)
import           Utils.PreludePlus
import           Utils.Vector

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.JSONInstances ()


data DragHistory = DragHistory { _dragStartPos  :: Vector2 Int
                               , _draggedNodeId :: NodeId
                               , _nodesStartPos :: Map NodeId (Vector2 Double)
                               } deriving (Eq, Show, Generic)

data State = State { _history :: Maybe DragHistory
                   } deriving (Eq, Show, Generic)

makeLenses ''State
makeLenses ''DragHistory

instance ToJSON State
instance ToJSON DragHistory

instance Default State where
    def = State def
