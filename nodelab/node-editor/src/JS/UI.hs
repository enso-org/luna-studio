{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.UI
    ( focus
    , setCursor
    , setDefaultCursor
    , setMovingCursor
    , isFocusInApp
    ) where

import qualified JS.Config           as Config
import           Luna.Prelude

foreign import javascript safe "document.getElementById($1).focus()" focus :: JSString -> IO ()

foreign import javascript safe "document.body.style.cursor = \"$1\";" setCursor' :: JSString -> IO ()

foreign import javascript safe "document.activeElement.id" getFocus :: IO JSString

isFocusInApp :: IO Bool
isFocusInApp = Config.isPrefixed <$> getFocus

setCursor :: MonadIO m => JSString -> m ()
setCursor = liftIO . setCursor'

setDefaultCursor, setMovingCursor :: MonadIO m => m ()
setDefaultCursor = setCursor "auto"
setMovingCursor = setCursor "col-resize"
