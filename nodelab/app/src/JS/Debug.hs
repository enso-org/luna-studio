module JS.Debug where

import           Data.Aeson          (ToJSON, toJSON)
import           Data.JSString.Text  (textToJSString)
import           Luna.Studio.Prelude

foreign import javascript safe "window.state  = $1"         saveState :: JSVal -> IO ()
foreign import javascript safe "window.lastEv = $1"         lastEv    :: JSVal -> IO ()
foreign import javascript safe "console.log($1)"            clog      :: JSVal -> IO ()
foreign import javascript safe "console.info('Incoming message', $1)" cinfo     :: JSVal -> IO ()
foreign import javascript safe "console.error($1, $2)"      error'    :: JSString -> JSVal -> IO ()
foreign import javascript safe "config.exportState" shouldExportState :: Bool
foreign import javascript safe "window.processedEvents.push($1)" processedEvent :: Int -> IO ()

error :: (ToJSON o) => Text -> o -> IO ()
error msg o = do
    let msg' = textToJSString msg
    o' <- toJSVal $ toJSON o
    error' msg' o'
