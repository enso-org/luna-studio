module JS.MultiSelection
    ( displaySelectionBox
    , hideSelectionBox
    ) where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

foreign import javascript safe "require('Selection').show($1, $2, $3, $4)"
    displaySelectionBoxJS :: Double -> Double -> Double -> Double -> IO ()

displaySelectionBox :: Vector2 Double -> Vector2 Double -> IO ()
displaySelectionBox (Vector2 x0 y0) (Vector2 x1 y1) = displaySelectionBoxJS x0 y0 x1 y1

foreign import javascript safe "require('Selection').hide()"
    hideSelectionBox :: IO ()
