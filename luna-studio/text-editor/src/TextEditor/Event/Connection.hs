{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Connection where

import           Common.Analytics                  (IsTrackedEvent)
import           Common.Batch.Connector.Connection (WebMessage)
import           Common.Data.Event                 (EventName)
import           Common.Prelude
import           Data.Aeson                        (ToJSON, toJSON)


data Event = Message { _message :: WebMessage }
           | Opened
           | Closed  { _code :: Int }
           | Error
             deriving (Eq, Generic, NFData, Show, Typeable)

makeLenses ''Event

instance ToJSON Event
instance ToJSON WebMessage where
    toJSON _ = toJSON "(webmessage)"
instance IsTrackedEvent Event
instance EventName Event
