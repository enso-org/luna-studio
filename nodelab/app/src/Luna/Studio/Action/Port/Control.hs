{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Port.Control
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    ) where

import           Data.ScreenPosition                 (ScreenPosition, x)
import           Empire.API.Data.PortDefault         (PortDefault (Constant), PortValue (DoubleValue, IntValue))
import           Empire.API.Data.PortRef             (AnyPortRef)
import qualified JS.UI                               as JS
import           Luna.Studio.Action.Basic            (localSetPortDefault)
import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.Action     (beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import           Luna.Studio.Action.State.NodeEditor (getPortDefault)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Action            (Action (begin, continue, end, update), InitValue (Continous, Discrete),
                                                      SliderDrag (SliderDrag), sliderDragAction, sliderDragInitValue, sliderDragPortRef,
                                                      sliderDragStartPos)
import           Luna.Studio.State.Global            (State)


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


startMoveSlider :: AnyPortRef -> ScreenPosition -> InitValue -> Command State ()
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
