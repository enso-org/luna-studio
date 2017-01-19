module Event.Event where

import           Data.Aeson          (ToJSON)

import qualified Event.Batch         as Batch
import qualified Event.Clipboard     as Clipboard
import qualified Event.Connection    as Connection
import qualified Event.CustomEvent   as CustomEvent
import qualified Event.Debug         as Debug
import           Event.UI            (UIEvent)
import           Luna.Studio.Prelude



data Event = Init
           | Batch                         Batch.Event
           | Clipboard                 Clipboard.Event
           | Connection               Connection.Event
           | CustomEvent             CustomEvent.Event
           | Debug                         Debug.Event
           | Tick
           | UI                                UIEvent
           deriving (Generic, Show)

makeLenses ''Event

instance Default Event where
    def = Init

instance ToJSON Event

name :: Getter Event String
name = to $ \n -> case n of
    Batch         _   -> "Batch"
    Clipboard     _   -> "Clipboard"
    Connection    _   -> "Connection"
    CustomEvent   _   -> "CustomEvent"
    Debug         _   -> "Debug"
    Init              -> "Init"
    Tick              -> "Tick"
    UI            _   -> "UI"
