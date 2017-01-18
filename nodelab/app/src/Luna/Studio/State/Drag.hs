module Luna.Studio.State.Drag where

import           Data.Aeson              (ToJSON)
import           Data.Map                (Map)
import           Luna.Studio.Data.Vector (Position)
import           Luna.Studio.Prelude

import           Empire.API.Data.Node    (NodeId)


data State = State { _dragStartPos  :: Position
                   , _draggedNodeId :: NodeId
                   , _nodesStartPos :: Map NodeId (Position)
                   } deriving (Eq, Show, Generic)

makeLenses ''State
instance ToJSON State
