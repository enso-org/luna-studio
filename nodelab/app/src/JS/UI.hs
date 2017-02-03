{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.UI
    ( displayConnectionClosedMessage
    , focus
    , setCursor
    , setDefaultCursor
    , setMovingCursor
    ) where

import           Luna.Studio.Prelude

foreign import javascript safe "require('./BSOD').connectionClosed()" displayConnectionClosedMessage :: IO ()

foreign import javascript safe "document.getElementById($1).focus()" focus :: JSString -> IO ()

foreign import javascript safe "document.body.style.cursor = \"$1\";" setCursor' :: JSString -> IO ()

setCursor :: MonadIO m => JSString -> m ()
setCursor = liftIO . setCursor'

setDefaultCursor, setMovingCursor :: MonadIO m => m ()
setDefaultCursor = setCursor "auto"
setMovingCursor = setCursor "col-resize"
