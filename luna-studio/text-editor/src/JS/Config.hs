{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Config
  ( getBackendAddress
  ) where

import           Common.Prelude



foreign import javascript safe "config.backendAddress.listen"
    js_getListenAddress :: IO JSString

foreign import javascript safe "config.backendAddress.send"
    js_getSendAddress :: IO JSString

getBackendAddress :: IO (String, String)
getBackendAddress  = convert .: (,) <$> js_getListenAddress <*> js_getSendAddress
