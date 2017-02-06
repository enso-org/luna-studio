{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Engine.JSHandlers
    ( AddHandler(..)
    , atomHandler
    , customEventHandler
    , sceneResizeHandler
    , webSocketHandler
    ) where

import           Luna.Studio.Prelude                    hiding (on)

import           GHCJS.Marshal.Pure                     (pFromJSVal)
import           GHCJS.Prim                             (fromJSString)

import qualified JS.Atom                                as Atom
import qualified JS.CustomEvent                         as CustomEvent
import qualified JS.Scene                               as Scene
import qualified JS.WebSocket                           as WebSocket
import qualified Luna.Studio.Batch.Connector.Connection as Connection
import qualified Luna.Studio.Event.Connection           as Connection
import qualified Luna.Studio.Event.CustomEvent          as CustomEvent
import           Luna.Studio.Event.Event                (Event (Connection, CustomEvent, Shortcut, UI))
import           Luna.Studio.Event.UI                   (UIEvent (AppEvent))
import qualified Luna.Studio.React.Event.App            as App


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))

atomHandler :: AddHandler Event
atomHandler = AddHandler $ \h ->
    Atom.onEvent $ h . Shortcut

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
        let frame = Connection.deserialize payload
        mapM_ (h . Connection . Connection.Message) $ frame ^. Connection.messages
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
