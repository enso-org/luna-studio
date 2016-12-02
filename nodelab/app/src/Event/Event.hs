module Event.Event where

import           Data.Aeson          (ToJSON, toJSON)
import           GHCJS.Marshal.Pure  (PFromJSVal (..), PToJSVal (..))

import qualified Event.Batch         as Batch
import qualified Event.Clipboard     as Clipboard
import qualified Event.Connection    as Connection
import qualified Event.ConnectionPen as ConnectionPen
import qualified Event.CustomEvent   as CustomEvent
import qualified Event.Debug         as Debug
import qualified Event.Keyboard      as Keyboard
import qualified Event.Mouse         as Mouse
import qualified Event.NodeSearcher  as NodeSearcher
import qualified Event.TextEditor    as TextEditor
import           Event.UI            (UIEvent)
import qualified Event.Widget        as Widget
import qualified Event.Window        as Window
import           Utils.PreludePlus



newtype JSState = JSState JSVal deriving (PFromJSVal, PToJSVal)

instance Eq JSState where
    _ == _ = True

instance Show JSState where
    show _ = "JSState"

data Event = Init
           | Batch                         Batch.Event
           | Clipboard                 Clipboard.Event
           | Connection               Connection.Event
           | ConnectionPen         ConnectionPen.Event
           | CustomEvent             CustomEvent.Event
           | Debug                         Debug.Event
           | Keyboard      JSState      Keyboard.Event
           | Mouse         JSState      Mouse.RawEvent
           | NodeSearcher           NodeSearcher.Event
           | TextEditor               TextEditor.Event
           | Tick
           | UI                                UIEvent
           | Widget                       Widget.Event
           | Window                       Window.Event
           deriving (Generic, Show)

makeLenses ''Event

instance Default Event where
    def = Init

instance ToJSON Event

instance ToJSON JSState where
    toJSON _ = toJSON "(..)"

name :: Getter Event String
name = to $ \n -> case n of
    Batch         _   -> "Batch"
    Clipboard     _   -> "Clipboard"
    Connection    _   -> "Connection"
    ConnectionPen _   -> "ConnectionPen"
    CustomEvent   _   -> "CustomEvent"
    Debug         _   -> "Debug"
    Init              -> "Init"
    Keyboard      _ _ -> "Keyboard"
    Mouse         _ _ -> "Mouse"
    NodeSearcher  _   -> "NodeSearcher"
    TextEditor    _   -> "TextEditor"
    Tick              -> "Tick"
    UI            _   -> "UI"
    Widget        _   -> "Widget"
    Window        _   -> "Window"
