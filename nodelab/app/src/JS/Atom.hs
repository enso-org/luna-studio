{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( onEvent
    , pushCode
    ) where


import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure         (pFromJSVal)
import           Luna.Studio.Event.Shortcut (ShortcutEvent)
import           Luna.Studio.Prelude


foreign import javascript safe "atomCallback.pushCode($1)"
    pushCode :: JSString -> IO ()

foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()


onEvent :: (ShortcutEvent -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ callback . read . pFromJSVal
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback
