{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.Visualization where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Position                (Position)
import           React.Flux                   (MouseEvent)

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node (NodeLoc)



data Event = Pin                  NodeLoc Int
           | Unpin                NodeLoc Int Position
           | MouseDown MouseEvent NodeLoc Int Position
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
