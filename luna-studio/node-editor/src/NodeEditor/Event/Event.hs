{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.Event where

import           Common.Prelude
import qualified NodeEditor.Event.Atom       as Atom
import qualified NodeEditor.Event.Batch      as Batch
import qualified NodeEditor.Event.Connection as Connection
import           NodeEditor.Event.Shortcut   (ShortcutEvent)
import           NodeEditor.Event.UI         (UIEvent)


data Event = Init
           | Batch            Batch.Event
           | Connection  Connection.Event
           | Atom              Atom.Event
           | Shortcut       ShortcutEvent
           | UI                   UIEvent
           deriving (Generic, Show, NFData)

makeLenses ''Event

name :: Getter Event String
name = to $ head . words . show
