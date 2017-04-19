{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Node.Editor.Action.Port
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    , localSetPortDefault
    , handleMouseDown
    , handleClick
    , handleMouseEnter
    , handleMouseLeave
    ) where

import           Node.Editor.Action.Basic          (localSetPortDefault)
import           Node.Editor.Action.Port.Actions   (handleClick, handleMouseDown)
import           Node.Editor.Action.Port.Control   (moveSlider, startMoveSlider, stopMoveSlider)
import           Node.Editor.Action.Port.Highlight (handleMouseEnter, handleMouseLeave)
