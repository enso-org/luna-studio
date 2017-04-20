{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Event.Source
    ( AddHandler(..)
    , atomHandler
    , customEventHandler
    , sceneResizeHandler
    , webSocketHandler
    ) where

import           Common.Prelude                    hiding (on)

import           GHCJS.Marshal.Pure                     (pFromJSVal)
import           GHCJS.Prim                             (fromJSString)

import qualified JS.Atom                                as Atom
import qualified JS.CustomEvent                         as CustomEvent
import qualified JS.Scene                               as Scene
import qualified JS.UI                                  as UI
import qualified WebSocket                              as WebSocket
import qualified Common.Batch.Connector.Connection as BatchConnection
import qualified NodeEditor.Event.Connection           as Connection
import qualified NodeEditor.Event.CustomEvent          as CustomEvent
import           NodeEditor.Event.Event                (Event (Connection, CustomEvent, UI))
import           NodeEditor.Event.UI                   (UIEvent (AppEvent))
import qualified NodeEditor.React.Event.App            as App


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))

atomHandler :: AddHandler Event
atomHandler = AddHandler $ \h ->
    Atom.onEvent $ whenM UI.isFocusInApp . h

sceneResizeHandler :: AddHandler Event
sceneResizeHandler = AddHandler $ \h ->
    Scene.onSceneResize $ h $ UI $ AppEvent App.Resize

webSocketHandler :: WebSocket.WebSocket -> AddHandler Event
webSocketHandler conn = AddHandler $ \h -> do
    void $ WebSocket.onOpen conn $
        h $ Connection Connection.Opened
    void $ WebSocket.onMessage conn $ \event -> do
        payloadJS <- WebSocket.getData event
        let payload = fromJSString payloadJS
        -- liftIO $ putStrLn $ "payload len " <> show (length payload)
        let frame = BatchConnection.deserialize payload
        mapM_ (h . Connection . Connection.Message) $ frame ^. BatchConnection.messages
    void $ WebSocket.onClose conn $ \event -> do
        code <- WebSocket.getCode event
        h $ Connection $ Connection.Closed code
    WebSocket.onError conn $
        h $ Connection Connection.Error

customEventHandler :: AddHandler Event
customEventHandler  = AddHandler $ \h -> do
    CustomEvent.initializeEvents
    CustomEvent.registerCallback $ \topic payload ->
        liftIO $ h $ CustomEvent $ CustomEvent.RawEvent (pFromJSVal topic) payload
