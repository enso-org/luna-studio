{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Event.Event where

import           Data.Aeson                  (ToJSON)
import           Common.Prelude
import qualified NodeEditor.Event.Atom       as Atom
import qualified NodeEditor.Event.Batch      as Batch
import qualified NodeEditor.Event.Connection as Connection
import           NodeEditor.Event.Shortcut   (ShortcutEvent)
import           NodeEditor.Event.UI         (UIEvent)


data Event = Init
           | Batch                         Batch.Event
           | Connection               Connection.Event
           | Atom                           Atom.Event
           | Shortcut                    ShortcutEvent
           | UI                                UIEvent
           deriving (Generic, Show, NFData)

makeLenses ''Event

instance ToJSON Event

name :: Getter Event String
name = to $ head . words . show
