{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( pushCode
    , pushBuffer
    , pushStatus
    , subscribeText
    , subscribeEventListenerInternal
    ) where


import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure         (pFromJSVal)
import qualified Data.Text                  as Text
import           TextEditor.Event.Internal (InternalEvent, InternalEvent(..))
import           TextEditor.Event.Text (TextEvent, TextEvent (..))
import qualified TextEditor.Event.Text as TextEvent
import           Common.Prelude


foreign import javascript safe "atomCallbackTextEditor.pushCode($1, $2, $3, $4)"
    pushCode' :: JSString -> Int -> Int -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.pushBuffer($1, $2)"
    pushBuffer :: JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.pushStatus($1, $2, $3)"
    pushStatus :: JSString -> JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.subscribeEventListenerInternal($1)"
  subscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeEventListenerInternal()"
  unsubscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.subscribeText($1)"
  subscribeText' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeText()"
  unsubscribeText' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.getEvent($1)"
  getEvent :: JSVal -> JSVal

foreign import javascript safe "atomCallbackTextEditor.getPath($1)"
  getPath :: JSVal -> JSVal

foreign import javascript safe "atomCallbackTextEditor.getStart($1)"
  getStart :: JSVal -> JSVal

foreign import javascript safe "atomCallbackTextEditor.getStop($1)"
  getStop :: JSVal -> JSVal

foreign import javascript safe "atomCallbackTextEditor.getText($1)"
  getText :: JSVal -> JSVal

foreign import javascript safe "atomCallbackTextEditor.getCursor($1)"
  getCursor :: JSVal -> JSVal

foreign import javascript safe "atomCallbackInternals.getSelections($1)"
  getSelections :: JSVal -> JSVal

jsvalToText :: JSVal -> TextEvent
jsvalToText jsval = result where
  filepath = pFromJSVal $ getPath jsval
  start    = pFromJSVal $ getStart jsval
  stop     = pFromJSVal $ getStop jsval
  text     = pFromJSVal $ getText jsval
  cursor   = pFromJSVal $ getCursor jsval
  result   = TextEvent filepath start stop text $ Just cursor

jsvalToInternalEvent :: JSVal -> InternalEvent
jsvalToInternalEvent jsval = result where
    event = read $ pFromJSVal $ getEvent jsval
    filepath = pFromJSVal $ getPath jsval
    -- maybeSelections = GHCJSInternal.fromJSVal $ getSelections jsval
    result = InternalEvent event filepath Nothing

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
    wrappedCallback <- syncCallback1 ContinueAsync $ callback . jsvalToInternalEvent
    subscribeEventListenerInternal' wrappedCallback
    return $ unsubscribeEventListenerInternal' wrappedCallback >> releaseCallback wrappedCallback
