{-# LANGUAGE DeriveAnyClass #-}
module Node.Editor.Event.Event where

import           Data.Aeson                    (ToJSON)

import qualified Node.Editor.Event.Batch       as Batch
import qualified Node.Editor.Event.Connection  as Connection
import qualified Node.Editor.Event.CustomEvent as CustomEvent
import qualified Node.Editor.Event.Debug       as Debug
import           Node.Editor.Event.Shortcut    (ShortcutEvent)
import           Node.Editor.Event.UI          (UIEvent)
import           Luna.Prelude



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
