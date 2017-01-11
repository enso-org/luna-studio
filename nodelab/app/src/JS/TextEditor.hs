module JS.TextEditor
    ( setText
    , registerCallback
    , setWidth
    , setVisible
    , toJSString
    ) where

import           Data.JSString.Text
import           GHCJS.Foreign.Callback
import           GHCJS.Types
import           Luna.Studio.Prelude



foreign import javascript safe "textEditor.setText($1)" setText' :: JSString -> IO ()

setText :: Text -> IO ()
setText = setText' . textToJSString

foreign import javascript safe "textEditor.callback = $1"
    registerCallback' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "textEditor.callback = function(){ return null; }"
    unregisterCallback' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$r = $1" toJSString :: JSVal -> JSString

registerCallback :: (JSVal -> IO ()) -> IO (IO ())
registerCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerCallback' wrappedCallback
    return $ unregisterCallback' wrappedCallback >> releaseCallback wrappedCallback

foreign import javascript safe "textEditor.setWidth($1)" setWidth :: Int -> IO ()
foreign import javascript safe "textEditor.setVisible($1)" setVisible :: Bool -> IO ()
