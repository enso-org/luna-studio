{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}

module NodeEditor.React.Event.Connection where

import           Common.Data.Event                 (EventName)
import           Common.Prelude
import           NodeEditor.React.Model.Connection (ConnectionId)
import           React.Flux                        (MouseEvent)


data ModifiedEnd = Source | Destination
        deriving (Eq, Generic, NFData, Show, Typeable)

data Event = MouseDown MouseEvent ConnectionId ModifiedEnd
        deriving (Show, Generic, NFData, Typeable)

instance EventName Event
