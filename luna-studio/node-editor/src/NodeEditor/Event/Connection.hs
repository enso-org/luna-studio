{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Event.Connection where

import           Common.Batch.Connector.Connection (WebMessage)
import           Common.Prelude

data Event = Message { _message :: WebMessage }
           | Opened
           | Closed  { _code :: Int }
           | Error
             deriving (Eq, Generic, NFData, Show, Typeable)

makeLenses ''Event
