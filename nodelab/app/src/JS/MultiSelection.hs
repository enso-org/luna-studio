module JS.MultiSelection
    ( displaySelectionBox
    , hideSelectionBox
    ) where

import           Luna.Studio.Data.Vector (Position)
import           Luna.Studio.Prelude

foreign import javascript safe "require('Selection').show($1, $2, $3, $4)"
    displaySelectionBoxJS :: Double -> Double -> Double -> Double -> IO ()

displaySelectionBox :: Position -> Position -> IO ()
displaySelectionBox (Position x0 y0) (Position x1 y1) = displaySelectionBoxJS x0 y0 x1 y1

foreign import javascript safe "require('Selection').hide()"
    hideSelectionBox :: IO ()
