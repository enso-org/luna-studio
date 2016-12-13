{-# LANGUAGE OverloadedStrings #-}

module Reactive.Handlers
    ( mouseDownHandler
    , mouseUpHandler
    , mouseMovedHandler
    , mouseDblClickHandler
    , AddHandler(..)
    , getJSState
    , resizeHandler
    , mouseWheelHandler
    , keyDownHandler
    , keyUpHandler
    , keyPressedHandler
    , webSocketHandler
    , connectionPenHandler
    , textEditorHandler
    , customEventHandler
    , copyClipboardHandler
    , cutClipboardHandler
    , pasteClipboardHandler
    ) where

import           Luna.Studio.Prelude         hiding (on)
import           Luna.Studio.Data.Vector              (Vector2 (Vector2))

import           GHCJS.DOM                 (currentDocument, currentWindow)
import qualified GHCJS.DOM.Document        as Document
import           GHCJS.DOM.Element         (Element, dblClick, keyDown, keyPress, keyUp, mouseDown, mouseMove, mouseUp, wheel)
import           GHCJS.DOM.EventM          (EventM, EventName)
import qualified GHCJS.DOM.EventM          as EventM
import qualified GHCJS.DOM.KeyboardEvent   as KeyboardEvent
import qualified GHCJS.DOM.MouseEvent      as MouseEvent
import qualified GHCJS.DOM.UIEvent         as UIEvent
import qualified GHCJS.DOM.WheelEvent      as WheelEvent
import           GHCJS.DOM.Window          (getInnerHeight, getInnerWidth, resize)
import           GHCJS.Marshal             (fromJSValUnchecked)
import           GHCJS.Marshal.Pure        (pFromJSVal)
import           GHCJS.Prim                (fromJSString)
import qualified JavaScript.Array          as JSArray

import qualified Luna.Studio.Batch.Connector.Connection as Connection
import qualified Data.JSString             as JSString
import           Data.JSString.Text        (lazyTextFromJSString)
import qualified Event.Clipboard           as Clipboard
import qualified Event.Connection          as Connection
import qualified Event.ConnectionPen       as ConnectionPen
import qualified Event.CustomEvent         as CustomEvent
import           Event.Event
import qualified Event.Keyboard            as Keyboard
import qualified Event.Mouse               as Mouse
import qualified Event.TextEditor          as TextEditor
import qualified Event.Window              as Window
import qualified JS.Clipboard              as Clipboard
import qualified JS.ConnectionPen          as ConnectionPen
import qualified JS.CustomEvent            as CustomEvent
import qualified JS.TextEditor             as TextEditor
import qualified JS.WebSocket              as WebSocket
import           Object.UITypes            as Mouse
import           UI.Raycaster              hiding (widgetMatrix)


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))

foreign import javascript safe "require('common')" getJSState :: IO JSState

backspace :: Int
backspace = 8

readKeyMods :: (MouseEvent.IsMouseEvent e) => EventM t e Keyboard.KeyMods
readKeyMods = do
    e      <- EventM.event
    shift  <- MouseEvent.getShiftKey e
    ctrl   <- MouseEvent.getCtrlKey  e
    alt    <- MouseEvent.getAltKey   e
    meta   <- MouseEvent.getMetaKey  e
    return $  Keyboard.KeyMods shift ctrl alt meta

readKeyMods' :: EventM t KeyboardEvent.KeyboardEvent Keyboard.KeyMods
readKeyMods' = do
    e      <- EventM.event
    shift  <- KeyboardEvent.getShiftKey e
    ctrl   <- KeyboardEvent.getCtrlKey  e
    alt    <- KeyboardEvent.getAltKey   e
    meta   <- KeyboardEvent.getMetaKey  e
    return $  Keyboard.KeyMods shift ctrl alt meta

readMousePos :: (MouseEvent.IsMouseEvent e) => EventM t e Mouse.MousePosition
readMousePos = do
    e <- EventM.event
    x <- MouseEvent.getClientX e
    y <- MouseEvent.getClientY e
    return $ Vector2 x y

uiWhichButton :: (UIEvent.IsUIEvent e) => EventM t e Mouse.MouseButton
uiWhichButton = EventM.uiWhich >>= return . Mouse.toMouseButton

eventObject :: IO (Maybe Element)
eventObject = do
    doc <- fromJust <$> currentDocument
    Document.getElementById doc (JSString.pack "canvas2d")

mouseHandler :: EventName Element MouseEvent.MouseEvent -> Mouse.Type -> AddHandler Event
mouseHandler event tag =
    AddHandler $ \h -> do
        window <- fromJust <$> eventObject
        window `EventM.on` event $ do
            mousePos        <- readMousePos
            button          <- uiWhichButton
            keyMods         <- readKeyMods
            objectId        <- liftIO $ readObjectId     mousePos
            scene           <- liftIO $ whichScene       objectId
            widgetMatrix    <- liftIO $ readWidgetMatrix objectId
            let maybeWidget  = do justObjectId        <- objectId
                                  justWidgetMatrix    <- widgetMatrix
                                  justScene           <- scene
                                  return $ Mouse.EventWidget justObjectId justWidgetMatrix justScene
            jsState <- liftIO getJSState
            liftIO . h $ Mouse jsState $ Mouse.Event tag mousePos button keyMods maybeWidget

mouseDownHandler, mouseUpHandler, mouseMovedHandler, mouseDblClickHandler :: AddHandler Event
mouseDownHandler     = mouseHandler mouseDown  Mouse.Pressed
mouseUpHandler       = mouseHandler mouseUp    Mouse.Released
mouseMovedHandler    = mouseHandler mouseMove  Mouse.Moved
mouseDblClickHandler = mouseHandler dblClick   Mouse.DblClicked

mouseWheelHandler :: AddHandler Event
mouseWheelHandler =
    AddHandler $ \h -> do
        window <- fromJust <$> eventObject
        window `EventM.on` wheel $ do
            EventM.preventDefault
            mousePos        <- readMousePos
            button          <- uiWhichButton
            keyMods         <- readKeyMods

            objectId        <- liftIO $ readObjectId     mousePos
            scene           <- liftIO $ whichScene       objectId
            widgetMatrix    <- liftIO $ readWidgetMatrix objectId
            delta <- do
                e <- EventM.event
                x <- WheelEvent.getDeltaX e
                y <- WheelEvent.getDeltaY e
                return $ Vector2 x y


            let maybeWidget  = do justObjectId        <- objectId
                                  justWidgetMatrix    <- widgetMatrix
                                  justScene           <- scene
                                  return $ Mouse.EventWidget justObjectId justWidgetMatrix justScene
            jsState <- liftIO getJSState
            liftIO . h $ Mouse jsState $ Mouse.Event (Mouse.Wheel delta) mousePos button keyMods maybeWidget

keyHandler :: EventName Element KeyboardEvent.KeyboardEvent -> EventM Element KeyboardEvent.KeyboardEvent Int -> Keyboard.Type -> AddHandler Event
keyHandler event getter tag = AddHandler $ \h -> do
    window <- fromJust <$> eventObject
    window `EventM.on` event $ do
        key     <- getter
        when (key == backspace) EventM.preventDefault
        keyMods <- readKeyMods'
        jsState <- liftIO getJSState
        liftIO . h $ Keyboard jsState $ Keyboard.Event tag (chr key) keyMods

keyPressedHandler, keyDownHandler, keyUpHandler :: AddHandler Event
keyPressedHandler = keyHandler keyPress EventM.uiCharCode Keyboard.Press
keyDownHandler    = keyHandler keyDown  EventM.uiKeyCode  Keyboard.Down
keyUpHandler      = keyHandler keyUp    EventM.uiKeyCode  Keyboard.Up

resizeHandler :: AddHandler Event
resizeHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `EventM.on` resize $ liftIO $ do
        width  <- getInnerWidth  window
        height <- getInnerHeight window
        h $ Window $ Window.Resized width height

webSocketHandler :: WebSocket.WebSocket -> AddHandler Event
webSocketHandler conn = AddHandler $ \h -> do
    void $ WebSocket.onOpen conn $
        h $ Connection Connection.Opened
    void $ WebSocket.onMessage conn $ \event -> do
        payloadJS <- WebSocket.getData event
        let payload = fromJSString payloadJS
        -- liftIO $ putStrLn $ "payload len " <> show (length payload)
        let frame = Connection.deserialize payload
        mapM_ (h . Connection . Connection.Message) $ frame ^. Connection.messages
    void $ WebSocket.onClose conn $ \event -> do
        code <- WebSocket.getCode event
        h $ Connection $ Connection.Closed code
    WebSocket.onError conn $
        h $ Connection Connection.Error

connectionPenHandler :: AddHandler Event
connectionPenHandler  = AddHandler $ \h ->
    ConnectionPen.registerCallback $ \widgets -> do
        let arr   =  JSArray.toList (ConnectionPen.toJSArray widgets)
        widgetIds <- mapM fromJSValUnchecked arr :: IO [Int]
        liftIO $ h $ ConnectionPen $ ConnectionPen.Segment $ map WidgetId widgetIds

textEditorHandler :: AddHandler Event
textEditorHandler  = AddHandler $ \h ->
    TextEditor.registerCallback $ \code -> do
        let codeStr = TextEditor.toJSString code
        liftIO $ h $ TextEditor $ TextEditor.CodeModified $ lazyTextFromJSString codeStr

customEventHandler :: AddHandler Event
customEventHandler  = AddHandler $ \h -> do
    CustomEvent.initializeEvents
    CustomEvent.registerCallback $ \topic payload ->
        liftIO $ h $ CustomEvent $ CustomEvent.RawEvent (JSString.unpack $ pFromJSVal topic) payload

copyClipboardHandler :: AddHandler Event
copyClipboardHandler =
  AddHandler $ \h -> do
    Clipboard.registerCopyCallback $ \_ ->
      liftIO . h $ Clipboard $ Clipboard.Copy

cutClipboardHandler :: AddHandler Event
cutClipboardHandler =
  AddHandler $ \h -> do
    Clipboard.registerCutCallback $ \_ ->
      liftIO . h $ Clipboard $ Clipboard.Cut

pasteClipboardHandler :: AddHandler Event
pasteClipboardHandler =
  AddHandler $ \h -> do
    Clipboard.registerPasteCallback $ \jsval ->
      liftIO . h $ Clipboard $ Clipboard.Paste (lazyTextFromJSString $ pFromJSVal jsval)
