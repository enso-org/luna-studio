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
    , editTextPortControl
    , acceptEditTextPortControl
    , unfocusEditTextPortControl
    , rollbackEditTextPortControl
    ) where

import           NodeEditor.Action.Basic          (localSetPortDefault)
import           NodeEditor.Action.Port.Actions   (handleClick, handleMouseDown)
import           NodeEditor.Action.Port.Control   (acceptEditTextPortControl, editTextPortControl, moveSlider, rollbackEditTextPortControl,
                                                   startMoveSlider, stopMoveSlider, unfocusEditTextPortControl)
import           NodeEditor.Action.Port.Highlight (handleMouseEnter, handleMouseLeave)
