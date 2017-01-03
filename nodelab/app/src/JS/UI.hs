{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.UI
    ( displayConnectionClosedMessage
    ) where

import           Luna.Studio.Prelude

foreign import javascript safe "require('BSOD').connectionClosed()" displayConnectionClosedMessage :: IO ()
