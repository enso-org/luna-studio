{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.Visualization where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Position                (Position)
import           React.Flux                   (MouseEvent)

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node (NodeId)



data Event = Pin                  NodeId Int
           | Unpin                NodeId Int Position
           | MouseDown MouseEvent NodeId Int Position
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
