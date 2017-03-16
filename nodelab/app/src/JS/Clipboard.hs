{-# LANGUAGE JavaScriptFFI #-}

module JS.Clipboard
    ( getClipboardData
    , copyStringToClipboard
    ) where

import           Luna.Studio.Prelude
import           React.Flux.Internal (HandlerArg (HandlerArg))
import           System.IO.Unsafe    (unsafePerformIO)


foreign import javascript safe "$1.clipboardData.getData('Text')" getClipboardData' :: HandlerArg -> IO JSString

getClipboardData :: HandlerArg -> String
getClipboardData = unsafePerformIO . fmap convert . getClipboardData'

foreign import javascript safe "copyToClipboard($1)" copyStringToClipboard :: JSString -> IO ()
