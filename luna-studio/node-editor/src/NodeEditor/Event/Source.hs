{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Event.Source
    ( AddHandler(..)
    , atomHandler
    , sceneResizeHandler
    , webSocketHandler
    ) where

import           Common.Prelude                    hiding (on)

import           GHCJS.Prim                        (fromJSString)

import qualified Common.Batch.Connector.Connection as BatchConnection
import qualified JS.Atom                           as Atom
import qualified JS.Scene                          as Scene
import qualified JS.UI                             as UI
import qualified NodeEditor.Event.Connection       as Connection
import           NodeEditor.Event.Event            (Event (Atom, Connection, UI))
import           NodeEditor.Event.UI               (UIEvent (AppEvent))
import qualified NodeEditor.React.Event.App        as App
import qualified WebSocket                         as WebSocket


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))

atomHandler :: AddHandler Event
atomHandler = AddHandler $ \h ->
    Atom.onEvent $ \ev -> case ev of
        Atom {} -> h ev
        _       -> whenM UI.isFocusInApp $ h ev

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
