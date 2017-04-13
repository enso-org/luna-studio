{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Config
  ( getBackendAddress
  ) where

import qualified Data.JSString       as JSString
import qualified Data.List           as List
import           GHCJS.Marshal.Pure  (pFromJSVal)
import           Internal.Prelude
import           System.IO.Unsafe    (unsafePerformIO)



foreign import javascript safe "config.backendAddress"
    getBackendAddress' :: IO JSString

getBackendAddress :: IO String
getBackendAddress  = convert <$> getBackendAddress'
