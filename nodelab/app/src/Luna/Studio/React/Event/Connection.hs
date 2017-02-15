{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Connection where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Empire.API.Data.Connection (ConnectionId)
import           Luna.Studio.Prelude
import           React.Flux                 (MouseEvent)



data ModifiedEnd = Source | Destination deriving (Eq, Generic, NFData, Show, Typeable)

instance ToJSON   ModifiedEnd
instance FromJSON ModifiedEnd

data Event = MouseDown MouseEvent ConnectionId ModifiedEnd deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
