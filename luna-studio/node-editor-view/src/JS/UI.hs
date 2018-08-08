{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.UI
    ( isFocusInApp
    , lockCursor
    , setCursor
    , setDefaultCursor
    , setMovingCursor
    , unlockCursor
    ) where

import           Common.Prelude
import qualified JS.Mount       as Mount
import           JS.Scene       (planeCanvasId)

foreign import javascript safe "document.activeElement.id"            getFocus   :: IO JSString
foreign import javascript safe "document.body.style.cursor = \"$1\";" setCursor' :: JSString -> IO ()

foreign import javascript safe "document.getElementById($1).requestPointerLock(); document.addEventListener(\"mousemove\", movementHandler, false);"
    lockCursor'   :: JSString -> IO ()
foreign import javascript safe "document.exitPointerLock(); document.removeEventListener(\"mousemove\", movementHandler, false);"
    unlockCursor' :: IO ()

isFocusInApp :: IO Bool
isFocusInApp = Mount.isPrefixed <$> getFocus

setCursor :: MonadIO m => JSString -> m ()
setCursor = liftIO . setCursor'

setDefaultCursor, setMovingCursor :: MonadIO m => m ()
setDefaultCursor = setCursor "auto"
setMovingCursor  = setCursor "col-resize"

lockCursor, unlockCursor :: MonadIO m => m ()
lockCursor   = liftIO $ lockCursor' planeCanvasId
unlockCursor = liftIO unlockCursor'
