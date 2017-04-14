module JS.CustomEvent
    ( registerCallback
    , initializeEvents
    ) where

import           GHCJS.Foreign.Callback
import           GHCJS.Types
import           Internal.Prelude

foreign import javascript safe "require('CustomEvent').initializeEvents()"
    initializeEvents :: IO ()

foreign import javascript safe "require('CustomEvent').customEvent = $1"
    registerCallback' :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

foreign import javascript safe "require('CustomEvent').customEvent = function(){ return null; }"
    unregisterCallback' :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

registerCallback :: (JSVal -> JSVal -> IO ()) -> IO (IO ())
registerCallback callback = do
    wrappedCallback <- asyncCallback2 callback
    registerCallback' wrappedCallback
    return $ unregisterCallback' wrappedCallback >> releaseCallback wrappedCallback
