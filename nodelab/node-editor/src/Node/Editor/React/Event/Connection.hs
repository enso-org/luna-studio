{-# LANGUAGE DeriveAnyClass #-}

module Node.Editor.React.Event.Connection where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Luna.Prelude
import           Node.Editor.React.Model.Connection (ConnectionId)
import           React.Flux                         (MouseEvent)



data ModifiedEnd = Source | Destination deriving (Eq, Generic, NFData, Show, Typeable)

instance ToJSON   ModifiedEnd
instance FromJSON ModifiedEnd

data Event = MouseDown MouseEvent ConnectionId ModifiedEnd deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
