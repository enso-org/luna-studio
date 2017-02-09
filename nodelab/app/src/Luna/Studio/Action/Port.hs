{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Port
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    , setPortDefault
    , handleMouseEnter
    , handleMouseLeave
    , removeSelfIfNeeded
    ) where

import           Luna.Studio.Action.Port.Control   (moveSlider, setPortDefault, startMoveSlider, stopMoveSlider)
import           Luna.Studio.Action.Port.Highlight (handleMouseEnter, handleMouseLeave)
import           Luna.Studio.Action.Port.Self      (removeSelfIfNeeded)
