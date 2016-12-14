{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Camera where

import           Luna.Studio.Prelude

foreign import javascript safe "common.camera.updateProjectionMatrix()"
    updateProjectionMatrix :: IO ()

foreign import javascript safe "common.cameraHUD.updateProjectionMatrix()"
    updateHUDProjectionMatrix :: IO ()

foreign import javascript safe "require('Rendering').updateScreenSize($1, $2)"
    updateScreenSize :: Int -> Int -> IO ()

foreign import javascript safe "require('Rendering').updateHtmCanvasPanPos($1, $2, $3)"
    updateHtmCanvasPanPos :: Double -> Double -> Double -> IO ()

foreign import javascript safe "require('Rendering').updateCamera($1, $2, $3, $4, $5)"
    updateCamera :: Double -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript safe "require('Rendering').updateCameraHUD($1, $2, $3, $4)"
    updateCameraHUD :: Double -> Double -> Double -> Double -> IO ()

