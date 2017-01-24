{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( onEvent
    ) where


import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure     (pFromJSVal, pToJSVal)
import           Luna.Studio.Event.Atom (AtomEvent)
import           Luna.Studio.Prelude



foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()


onEvent :: (AtomEvent -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ callback . read . pFromJSVal
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback
