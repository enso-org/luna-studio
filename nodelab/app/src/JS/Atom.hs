{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( onEvent
    , openedFile
    , mountPoint
    ) where

import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure         (pFromJSVal)
import           Luna.Studio.Event.Shortcut (ShortcutEvent)
import           Luna.Studio.Prelude


foreign import javascript safe "arg_url" openedFile' :: IO JSVal
foreign import javascript safe "arg_mount" mountPoint' :: IO JSVal

openedFile :: IO (Maybe String)
openedFile = pFromJSVal <$> openedFile'

mountPoint :: IO (Maybe String)
mountPoint = pFromJSVal <$> mountPoint'

foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()


onEvent :: (ShortcutEvent -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ callback . read . pFromJSVal
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback
