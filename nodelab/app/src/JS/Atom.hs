{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( onEvent
    , pushCode
    , pushNotification
    -- , subscribeEventListenerInternal
    ) where
import GHCJS.Types (JSVal)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure         (pFromJSVal, pToJSVal)
import           Luna.Studio.Error.Error
import           Luna.Studio.Event.Shortcut (ShortcutEvent)
import qualified Luna.Studio.Event.Shortcut as Shortcut
import           Luna.Studio.Event.Internal (InternalEvent)
import           Luna.Studio.Event.Internal as Internal
import           Luna.Studio.Prelude


foreign import javascript safe "atomCallback.pushNotification($1, $2)"
  pushNotification' :: Int -> JSString -> IO ()

foreign import javascript safe "atomCallback.pushCode($1)"
    pushCode :: JSString -> IO ()

foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallback.subscribeEventListenerInternal($1)"
  subscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unsubscribeEventListenerInternal()"
  unsubscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

onEvent :: (ShortcutEvent -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ callback . Shortcut.fromString . pFromJSVal
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback

pushNotification :: Notification -> IO ()
pushNotification  = do
    num <- (^. notificationType)
    msg <- (^. notificationMsg)
    return $ pushNotification' (fromEnum num) (convert msg)
