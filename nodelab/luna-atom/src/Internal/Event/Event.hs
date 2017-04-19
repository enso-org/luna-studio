{-# LANGUAGE DeriveAnyClass #-}
module Internal.Event.Event where

import           Data.Aeson                    (ToJSON)

import qualified Internal.Event.Batch       as Batch
import qualified Internal.Event.Connection  as Connection
import qualified Internal.Event.CustomEvent as CustomEvent
import qualified Internal.Event.Debug       as Debug
import           Internal.Event.Internal    (InternalEvent)
-- import           Internal.Event.Shortcut    (ShortcutEvent)
import           Internal.Event.Text        (TextEvent)
-- import           Internal.Event.UI          (UIEvent)
import           Luna.Prelude



data Event = Init
           | Atom                        InternalEvent
           | Batch                         Batch.Event
           | Connection               Connection.Event
           | CustomEvent             CustomEvent.Event
           | Debug                         Debug.Event
           | Tick
           | Text                            TextEvent
           deriving (Generic, Show, NFData)


instance ToJSON Event

name :: Getter Event String
name = to $ head . words . show
