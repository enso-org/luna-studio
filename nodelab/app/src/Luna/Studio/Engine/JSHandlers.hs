{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Engine.JSHandlers
    ( AddHandler(..)
    , getJSState
    , resizeHandler
    , webSocketHandler
    , textEditorHandler
    , customEventHandler
    , copyClipboardHandler
    , cutClipboardHandler
    , pasteClipboardHandler
    ) where

import           Luna.Studio.Prelude                    hiding (on)

import           GHCJS.DOM                              (currentWindow)
import qualified GHCJS.DOM.EventM                       as EventM
import           GHCJS.DOM.Window                       (getInnerHeight, getInnerWidth, resize)
import           GHCJS.Marshal.Pure                     (pFromJSVal)
import           GHCJS.Prim                             (fromJSString)

import qualified Data.JSString                          as JSString
import           Data.JSString.Text                     (textFromJSString)
import qualified Event.Clipboard                        as Clipboard
import qualified Event.Connection                       as Connection
import qualified Event.CustomEvent                      as CustomEvent
import           Event.Event
import qualified Event.TextEditor                       as TextEditor
import qualified Event.Window                           as Window
import qualified JS.Clipboard                           as Clipboard
import qualified JS.CustomEvent                         as CustomEvent
import qualified JS.TextEditor                          as TextEditor
import qualified JS.WebSocket                           as WebSocket
import qualified Luna.Studio.Batch.Connector.Connection as Connection


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))

foreign import javascript safe "require('common')" getJSState :: IO JSState

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

textEditorHandler :: AddHandler Event
textEditorHandler  = AddHandler $ \h ->
    TextEditor.registerCallback $ \code -> do
        let codeStr = TextEditor.toJSString code
        liftIO $ h $ TextEditor $ TextEditor.CodeModified $ textFromJSString codeStr

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
      liftIO . h $ Clipboard $ Clipboard.Paste (textFromJSString $ pFromJSVal jsval)
