{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( pushCode
    , pushBuffer
    , subscribeText
    , subscribeEventListenerInternal
    ) where


import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure         (pFromJSVal)
import qualified Data.JSString              as JSString
import qualified Data.Text                  as Text
import           Luna.Studio.Event.Internal (InternalEvent)
import qualified Luna.Studio.Event.Internal as Internal
import           Luna.Studio.Event.Text (TextEvent, TextEvent (..))
import qualified Luna.Studio.Event.Text as TextEvent
import           Luna.Studio.Prelude


foreign import javascript safe "atomCallback2.pushCode($1, $2, $3, $4)"
    pushCode' :: JSString -> Int -> Int -> JSString -> IO ()

foreign import javascript safe "atomCallback2.pushBuffer($1)"
    pushBuffer :: JSString -> JSString -> IO ()


foreign import javascript safe "atomCallback2.subscribeEventListenerInternal($1)"
  subscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeEventListenerInternal()"
  unsubscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallback2.subscribeText($1)"
  subscribeText' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeText()"
  unsubscribeText' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "getPath($1)"
  getPath :: JSVal -> JSString

foreign import javascript safe "getStart($1)"
  getStart :: JSVal -> Int

foreign import javascript safe "getStop($1)"
  getStop :: JSVal -> Int

foreign import javascript safe "getText($1)"
  getText :: JSVal -> JSString

foreign import javascript safe "getCursor($1)"
  getCursor :: JSVal -> Int

jsvalToText :: JSVal -> TextEvent
jsvalToText jsval = result where
  filepath = JSString.unpack $ getPath jsval
  start    = getStart jsval
  stop     = getStop jsval
  text     = JSString.unpack $ getText jsval
  cursor   = getCursor jsval
  result   = TextEvent filepath start stop (Text.pack text) $ Just cursor

subscribeText :: (TextEvent -> IO ()) -> IO (IO ())
subscribeText callback = do
  wrappedCallback <- syncCallback1 ContinueAsync $ callback . jsvalToText
  subscribeText' wrappedCallback
  return $ unsubscribeText' wrappedCallback >> releaseCallback wrappedCallback

pushCode :: TextEvent -> IO ()
pushCode = do
  uri   <- (^. TextEvent.filePath)
  start <- (^. TextEvent.start)
  end   <- (^. TextEvent.stop)
  text  <- (^. TextEvent.text)
  return $ pushCode' (convert uri) start end $ convert $ Text.unpack text

subscribeEventListenerInternal :: (InternalEvent -> IO ()) -> IO (IO ())
subscribeEventListenerInternal callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ callback . Internal.fromString . pFromJSVal
    subscribeEventListenerInternal' wrappedCallback
    return $ unsubscribeEventListenerInternal' wrappedCallback >> releaseCallback wrappedCallback
