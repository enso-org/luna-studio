{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.Port
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    , localSetPortDefault
    , handleMouseDown
    , handleClick
    , handleMouseEnter
    , handleMouseLeave
    ) where

import           NodeEditor.Action.Basic          (localSetPortDefault)
import           NodeEditor.Action.Port.Actions   (handleClick, handleMouseDown)
import           NodeEditor.Action.Port.Control   (moveSlider, startMoveSlider, stopMoveSlider)
import           NodeEditor.Action.Port.Highlight (handleMouseEnter, handleMouseLeave)
