{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Port
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    , localSetPortDefault
    , handleMouseDown
    , handleClick
    , handleMouseEnter
    , handleMouseLeave
    ) where

import           Luna.Studio.Action.Basic          (localSetPortDefault)
import           Luna.Studio.Action.Port.Actions   (handleClick, handleMouseDown)
import           Luna.Studio.Action.Port.Control   (moveSlider, startMoveSlider, stopMoveSlider)
import           Luna.Studio.Action.Port.Highlight (handleMouseEnter, handleMouseLeave)
