{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.Visualization where

import           LunaStudio.Data.NodeValue   (VisualizerName)
import           LunaStudio.Data.Position    (Position)
import           React.Flux                  (MouseEvent)

import           Common.Prelude
import           NodeEditor.React.Model.Node (NodeLoc)



data Event = Focus               NodeLoc
           | SelectVisualization NodeLoc VisualizerName
            deriving (Show, Generic, NFData, Typeable)
