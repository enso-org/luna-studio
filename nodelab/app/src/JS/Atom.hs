{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( Event
    , onEvent
    ) where

import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure     (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types            (IsJSVal)
import           Luna.Studio.Prelude

newtype Event = Event JSVal deriving (PFromJSVal, PToJSVal)

instance IsJSVal Event


foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()


onEvent :: (Event -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync (callback . pFromJSVal)
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback
