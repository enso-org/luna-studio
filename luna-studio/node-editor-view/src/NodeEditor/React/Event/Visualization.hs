{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}
module NodeEditor.React.Event.Visualization where

import           Common.Data.Event                    (EventName, eventName)
import           Common.Prelude
import           LunaStudio.Data.NodeLoc              (NodeLoc)
import           LunaStudio.Data.NodeValue            (VisualizationId, VisualizerName)
import           NodeEditor.React.Model.Visualization (VisualizationParent, _Node)


data Event = Event { _visParent :: VisualizationParent
                   , _evtType   :: EventType
                   } deriving (Show, Generic, NFData, Typeable)

data EventType = Focus                VisualizationId
               | SelectVisualizer     VisualizationId VisualizerName
               | ToggleVisualizations
               deriving (Show, Generic, NFData, Typeable)

makeLenses ''Event
instance EventName EventType
instance EventName Event where
    eventName e = eventName $ e ^. evtType

mayNodeLoc :: Getter Event (Maybe NodeLoc)
mayNodeLoc = to (^? visParent . _Node)
