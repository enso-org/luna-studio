module JS.Debug where

import           Common.Prelude
import           Data.Aeson         (ToJSON, toJSON)
import           Data.JSString.Text (textToJSString)

foreign import javascript safe "console.error($1, $2)"      error'    :: JSString -> JSVal -> IO ()

error :: ToJSON o => Text -> o -> IO ()
error msg o = do
    let msg' = textToJSString msg
    o' <- toJSVal $ toJSON o
    error' msg' o'
