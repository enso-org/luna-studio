{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.Event where

import           Common.Analytics            (IsTrackedEvent (isTracked))
import           Common.Data.Event           (EventName (eventName), consName)
import           Common.Prelude
import qualified NodeEditor.Event.Atom       as Atom
import qualified NodeEditor.Event.Batch      as Batch
import qualified NodeEditor.Event.Connection as Connection
import           NodeEditor.Event.Shortcut   (ShortcutEvent)
import           NodeEditor.Event.UI         (UIEvent)

data Event = Init
           | Atom              Atom.Event
           | Batch            Batch.Event
           | Connection  Connection.Event
           | Shortcut       ShortcutEvent
           | UI                   UIEvent
           deriving (Generic, Show, NFData)

makeLenses ''Event


instance IsTrackedEvent Event where
    isTracked = \case
        Init          -> True
        Batch      ev -> isTracked ev
        Connection ev -> isTracked ev
        Atom       ev -> isTracked ev
        Shortcut   ev -> isTracked ev
        UI         ev -> isTracked ev

instance EventName Event where
    eventName event = "NodeEditor.Event." <> consName event <> case event of
        Init         -> def
        Atom a       -> "." <> eventName a
        Batch b      -> "." <> eventName b
        Connection c -> "." <> eventName c
        Shortcut s   -> "." <> eventName s
        UI u         -> "." <> eventName u
