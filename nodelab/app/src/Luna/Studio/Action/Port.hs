{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Port
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    , setPortDefault
    , setHighlight
    , removeSelfIfNeeded
    ) where

import           Luna.Studio.Action.Port.Control   (moveSlider, setPortDefault, startMoveSlider, stopMoveSlider)
import           Luna.Studio.Action.Port.Highlight (setHighlight)
import           Luna.Studio.Action.Port.Self      (removeSelfIfNeeded)