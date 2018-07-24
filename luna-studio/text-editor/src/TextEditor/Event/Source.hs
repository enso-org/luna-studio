{-# LANGUAGE OverloadedStrings #-}

module TextEditor.Event.Source
    ( AddHandler(..)
    , fileHandler
    , textHandler
    , webSocketHandler
    ) where

import           Common.Prelude                    hiding (on)

import qualified Common.Batch.Connector.Connection as BatchConnection
import qualified JS.Atom                           as Atom
import qualified TextEditor.Event.Connection       as Connection
import           TextEditor.Event.Event            (Event (Atom, Connection, Text))
import qualified WebSocket                         as WebSocket


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))


textHandler :: AddHandler Event
textHandler = AddHandler $ \h ->
    Atom.subscribeDiffs $ h . Text

fileHandler :: AddHandler Event
fileHandler = AddHandler $ \h -> do
    Atom.subscribeEventListenerInternal $ h . Atom


webSocketHandler :: WebSocket.WebSocket -> AddHandler Event
webSocketHandler conn = AddHandler $ \h -> do
    void $ WebSocket.onOpen conn $
        h $ Connection Connection.Opened
    void $ WebSocket.onMessage conn $ \event -> do
        payload <- WebSocket.getData event
        let frame = BatchConnection.deserialize payload
        mapM_ (h . Connection . Connection.Message) $ frame ^. BatchConnection.messages
    void $ WebSocket.onClose conn $ \event -> do
        code <- WebSocket.getCode event
        h $ Connection $ Connection.Closed code
    WebSocket.onError conn $
        h $ Connection Connection.Error
