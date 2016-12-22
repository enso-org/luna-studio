module Luna.Studio.State.Drag where

import           Data.Aeson               (ToJSON)
import           Data.Map                 (Map)
import           Luna.Studio.Data.Vector  (Position)
import           Luna.Studio.Prelude

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.JSONInstances ()


data DragHistory = DragHistory { _dragStartPos  :: Position
                               , _draggedNodeId :: NodeId
                               , _nodesStartPos :: Map NodeId (Position)
                               } deriving (Eq, Show, Generic)

data State = State { _history :: Maybe DragHistory
                   } deriving (Eq, Show, Generic)

makeLenses ''State
makeLenses ''DragHistory

instance ToJSON State
instance ToJSON DragHistory

instance Default State where
    def = State def
