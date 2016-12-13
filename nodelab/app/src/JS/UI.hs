{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.UI
    ( displayConnectionClosedMessage
    , calculateTextWidth
    , shouldRender
    , initializeGl
    , initializeHelp
    , render
    , triggerWindowResize
    ) where

import           Data.JSString.Text (lazyTextToJSString)
import           Luna.Studio.Prelude

foreign import javascript safe "require('Rendering').initialize()" initializeGl :: IO ()

foreign import javascript safe "require('Help').initialize()" initializeHelp :: IO ()

foreign import javascript safe "require('Rendering').render()" render :: IO ()

foreign import javascript safe "window.dispatchEvent(new Event('resize'))" triggerWindowResize :: IO ()

foreign import javascript safe "require('Rendering').shouldRender()" shouldRender :: IO ()

foreign import javascript safe "require('BSOD').connectionClosed()" displayConnectionClosedMessage :: IO ()

foreign import javascript safe "breadcrumb.calculateTextWidth($1)"
    calculateTextWidth' :: JSString -> Int

calculateTextWidth :: Text -> Int
calculateTextWidth = calculateTextWidth' . lazyTextToJSString
