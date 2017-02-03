{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Config (getBackendAddress, openedFile, mountPoint, prefix) where

import           GHCJS.Marshal.Pure  (pFromJSVal)
import           Luna.Studio.Prelude
import           System.IO.Unsafe    (unsafePerformIO)


foreign import javascript safe "config.backendAddress"
    getBackendAddress' :: IO JSString

getBackendAddress :: IO String
getBackendAddress  = convert <$> getBackendAddress'

foreign import javascript safe "arg_url" openedFile' :: IO JSVal
foreign import javascript safe "arg_mount" mountPoint' :: IO JSVal

openedFile :: Maybe String
openedFile = unsafePerformIO $ pFromJSVal <$> openedFile'

mountPoint :: String
mountPoint = unsafePerformIO $ fromMaybe "luna-studio-mount" . pFromJSVal <$> mountPoint'

prefix :: JSString -> JSString
prefix name = convert mountPoint <> "-" <> name
