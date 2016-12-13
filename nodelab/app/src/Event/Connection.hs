module Event.Connection where

import Luna.Studio.Prelude
import Luna.Studio.Batch.Connector.Connection (WebMessage)
import Data.Aeson (ToJSON, toJSON)

data Event = Message { _message :: WebMessage }
           | Opened
           | Closed  { _code :: Int }
           | Error
             deriving (Eq, Show, Typeable, Generic)

makeLenses ''Event

instance ToJSON Event
instance ToJSON WebMessage where
    toJSON _ = toJSON "(webmessage)"
