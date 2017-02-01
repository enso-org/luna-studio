{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Config (getBackendAddress) where

import           Luna.Studio.Prelude


foreign import javascript safe "config.backendAddress"
    getBackendAddress' :: IO JSString

getBackendAddress :: IO String
getBackendAddress  = convert <$> getBackendAddress'
