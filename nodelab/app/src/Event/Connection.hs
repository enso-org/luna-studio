module Event.Connection where

import Luna.Studio.Prelude
import BatchConnector.Connection (WebMessage)
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
