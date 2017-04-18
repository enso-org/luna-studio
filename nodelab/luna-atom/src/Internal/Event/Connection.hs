{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
module Internal.Event.Connection where

import           Data.Aeson                             (ToJSON, toJSON)
import           Luna.Batch.Connector.Connection (WebMessage)
import           Luna.Prelude

data Event = Message { _message :: WebMessage }
           | Opened
           | Closed  { _code :: Int }
           | Error
             deriving (Eq, Generic, NFData, Show, Typeable)

makeLenses ''Event

instance ToJSON Event
instance ToJSON WebMessage where
    toJSON _ = toJSON "(webmessage)"
