{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.Connection where

import           Common.Analytics                  (IsTrackedEvent)
import           Common.Data.Event                 (EventName)
import           Common.Batch.Connector.Connection (WebMessage)
import           Common.Prelude


data Event = Message { _message :: WebMessage }
           | Opened
           | Closed  { _code :: Int }
           | Error
             deriving (Eq, Generic, NFData, Show, Typeable)

makeLenses ''Event

instance EventName Event
instance IsTrackedEvent Event
