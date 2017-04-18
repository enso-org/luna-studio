{-# LANGUAGE OverloadedStrings #-}

module Internal.Event.Source
    ( AddHandler(..)
    , fileHandler
    , textHandler
    , customEventHandler
    , webSocketHandler
    ) where

import           Luna.Prelude                    hiding (on)

import           GHCJS.Marshal.Pure                     (pFromJSVal)
import           GHCJS.Prim                             (fromJSString)

import qualified JS.Atom                                as Atom
import qualified JS.CustomEvent                         as CustomEvent
-- import qualified JS.Scene                               as Scene
-- import qualified JS.UI                                  as UI
import qualified JS.WebSocket                           as WebSocket
import qualified Internal.Batch.Connector.Connection as BatchConnection
import qualified Internal.Event.Connection           as Connection
import qualified Internal.Event.CustomEvent          as CustomEvent
import           Internal.Event.Event                (Event (Atom, Connection, CustomEvent, Text))
-- import qualified Internal.React.Event.App            as App


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))


textHandler :: AddHandler Event
textHandler = AddHandler $ \h ->
    Atom.subscribeText $ h . Text

fileHandler :: AddHandler Event
fileHandler = AddHandler $ \h -> do
    Atom.subscribeEventListenerInternal $ h . Atom


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
