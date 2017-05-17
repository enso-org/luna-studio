{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.Visualization where

import           LunaStudio.Data.Position    (Position)
import           React.Flux                  (MouseEvent)

import           Common.Prelude
import           NodeEditor.React.Model.Node (NodeLoc)



data Event = Pin                  NodeLoc Int
           | Unpin                NodeLoc Int Position
           | MouseDown MouseEvent NodeLoc Int Position
            deriving (Show, Generic, NFData, Typeable)
