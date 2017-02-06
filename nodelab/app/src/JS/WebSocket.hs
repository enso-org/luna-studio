{-# LANGUAGE JavaScriptFFI #-}

module JS.WebSocket (WebSocket
                    , onOpen
                    , onMessage
                    , onClose
                    , onError
                    , getWebSocket
                    , connect
                    , send
                    , getData
                    , getCode
                    ) where

import           Data.JSString
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure     (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types            (IsJSVal)
import           Luna.Studio.Prelude

newtype WebSocket      = WebSocket      JSVal deriving (PFromJSVal, PToJSVal)
newtype WSMessageEvent = WSMessageEvent JSVal deriving (PFromJSVal, PToJSVal)
newtype WSClosedEvent  = WSClosedEvent  JSVal deriving (PFromJSVal, PToJSVal)

instance IsJSVal WebSocket
instance IsJSVal WSMessageEvent

foreign import javascript safe "$1.data"
    getData :: WSMessageEvent -> IO JSVal

foreign import javascript safe "$1.code"
    getCode :: WSClosedEvent -> IO Int

foreign import javascript safe "app.websocket"
    getWebSocket :: IO WebSocket

foreign import javascript safe "$1.onOpen($2)"
    onOpen' :: WebSocket -> Callback (IO ()) -> IO ()

foreign import javascript safe "$1.unOnOpen($2)"
    unOnOpen' :: WebSocket -> Callback (IO ()) -> IO ()

foreign import javascript safe "$1.onMessage($2)"
    onMessage' :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnMessage()"
    unOnMessage' :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.onClose($2)"
    onClose' :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnClose()"
    unOnClose' :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.onError($2)"
    onError' :: WebSocket -> Callback (IO ()) -> IO ()

foreign import javascript safe "$1.unOnError()"
    unOnError' :: WebSocket -> Callback (IO ()) -> IO ()

foreign import javascript safe "$1.send($2)"
    send :: WebSocket -> JSString -> IO ()

foreign import javascript safe "$1.connect($2)"
    connect' :: WebSocket -> JSString -> IO ()

connect :: WebSocket -> String -> IO ()
connect ws addr = connect' ws $ convert addr

onOpen :: WebSocket -> IO () -> IO (IO ())
onOpen ws callback = do
    wrappedCallback <- asyncCallback callback
    onOpen' ws wrappedCallback
    return $ unOnOpen' ws wrappedCallback >> releaseCallback wrappedCallback

onMessage :: WebSocket -> (WSMessageEvent -> IO ()) -> IO (IO ())
onMessage ws callback = do
    wrappedCallback <- asyncCallback1 (callback . pFromJSVal)
    onMessage' ws wrappedCallback
    return $ unOnMessage' ws wrappedCallback >> releaseCallback wrappedCallback

onClose :: WebSocket -> (WSClosedEvent -> IO ()) -> IO (IO ())
onClose ws callback = do
    wrappedCallback <- asyncCallback1 (callback . pFromJSVal)
    onClose' ws wrappedCallback
    return $ unOnClose' ws wrappedCallback >> releaseCallback wrappedCallback

onError :: WebSocket -> IO () -> IO (IO ())
onError ws callback = do
    wrappedCallback <- asyncCallback callback
    onError' ws wrappedCallback
    return $ unOnError' ws wrappedCallback >> releaseCallback wrappedCallback
