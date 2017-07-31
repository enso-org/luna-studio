{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}
module NodeEditor.React.Event.Visualization where

import           Common.Prelude
import           LunaStudio.Data.NodeValue   (VisualizationId, VisualizerName)
import           NodeEditor.React.Model.Node (NodeLoc)



data Event = Focus                NodeLoc VisualizationId
           | SelectVisualizer     NodeLoc VisualizationId VisualizerName
           | ToggleVisualizations NodeLoc
           deriving (Show, Generic, NFData, Typeable)
