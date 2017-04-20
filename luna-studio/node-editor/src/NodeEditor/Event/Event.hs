{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Event.Event where

import           Data.Aeson                    (ToJSON)

import qualified NodeEditor.Event.Batch       as Batch
import qualified NodeEditor.Event.Connection  as Connection
import qualified NodeEditor.Event.CustomEvent as CustomEvent
import qualified NodeEditor.Event.Debug       as Debug
import           NodeEditor.Event.Shortcut    (ShortcutEvent)
import           NodeEditor.Event.UI          (UIEvent)
import           Common.Prelude



data Event = Init
           | Batch                         Batch.Event
           | Connection               Connection.Event
           | CustomEvent             CustomEvent.Event
           | Debug                         Debug.Event
           | Tick
           | Shortcut                    ShortcutEvent
           | UI                                UIEvent
           deriving (Generic, Show, NFData)

makeLenses ''Event

instance ToJSON Event

name :: Getter Event String
name = to $ head . words . show
