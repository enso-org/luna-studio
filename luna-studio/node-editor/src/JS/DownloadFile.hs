module JS.DownloadFile (downloadFile) where

import           Data.JSString.Text  (textToJSString)
import           Common.Prelude

foreign import javascript safe "require('DownloadFile').downloadFile($2, $1)" downloadFile' :: JSString -> JSString -> IO ()

downloadFile :: Text -> Text -> IO ()
downloadFile name payload = downloadFile' (textToJSString name) (textToJSString payload)
