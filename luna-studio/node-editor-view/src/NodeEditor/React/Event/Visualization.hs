{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}
module NodeEditor.React.Event.Visualization where

import           Common.Data.Event                    (EventName)
import           Common.Prelude
import           LunaStudio.Data.NodeValue            (VisualizationId, VisualizerName)
import           NodeEditor.React.Model.Visualization (VisualizationParent)


data Event = Focus                VisualizationParent VisualizationId
           | SelectVisualizer     VisualizationParent VisualizationId VisualizerName
           | ToggleVisualizations VisualizationParent
           deriving (Show, Generic, NFData, Typeable)

instance EventName Event
