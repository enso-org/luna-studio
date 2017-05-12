{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.Port.Control
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    ) where

import           Data.ScreenPosition                 (ScreenPosition, x)
import           LunaStudio.Data.PortDefault         (PortDefault (Constant), PortValue (DoubleValue, IntValue))
import           LunaStudio.Data.PortRef             (InPortRef)
import qualified JS.UI                               as JS
import           NodeEditor.Action.Basic            (localSetPortDefault)
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.Action     (beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import           NodeEditor.Action.State.NodeEditor (getPortDefault)
import           Common.Prelude
import           NodeEditor.State.Action            (Action (begin, continue, end, update), InitValue (Continous, Discrete),
                                                      SliderDrag (SliderDrag), sliderDragAction, sliderDragInitValue, sliderDragPortRef,
                                                      sliderDragStartPos)
import           NodeEditor.State.Global            (State)


instance Action (Command State) SliderDrag where
    begin    = beginActionWithKey    sliderDragAction
    continue = continueActionWithKey sliderDragAction
    update   = updateActionWithKey   sliderDragAction
    end a = do
        JS.setDefaultCursor
        let portRef = a ^. sliderDragPortRef
        mayDefVal <- getPortDefault portRef
        withJust mayDefVal $ void <$> localSetPortDefault portRef
        removeActionFromState sliderDragAction


startMoveSlider :: InPortRef -> ScreenPosition -> InitValue -> Command State ()
startMoveSlider portRef initPos initVal = do
    begin $ SliderDrag portRef initPos initVal
    JS.setMovingCursor

moveSlider :: ScreenPosition -> SliderDrag -> Command State ()
moveSlider currentPostion state = void $ localSetPortDefault portRef portDef where
    portDef = newPortDefault currentPostion state
    portRef = state ^. sliderDragPortRef

stopMoveSlider :: ScreenPosition -> SliderDrag -> Command State ()
stopMoveSlider currentPostion state = do
    JS.setDefaultCursor
    let portRef = state ^. sliderDragPortRef
        portDef = newPortDefault currentPostion state
    Batch.setPortDefault portRef portDef
    removeActionFromState sliderDragAction

newPortDefault :: ScreenPosition -> SliderDrag -> PortDefault
newPortDefault currentPostion slider =
    let delta = currentPostion ^. x - slider ^. sliderDragStartPos . x
    in Constant $ case slider ^. sliderDragInitValue of
          Discrete  val -> IntValue    $ val + round delta
          Continous val -> DoubleValue $ val + delta
