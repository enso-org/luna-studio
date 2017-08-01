{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.Port.Control
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    ) where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           Data.Time.Clock                    (UTCTime)
import qualified Data.Time.Clock                    as Clock
import qualified JS.UI                              as JS
import           LunaStudio.Data.PortDefault        (PortDefault (Constant), PortValue (DoubleValue, IntValue))
import           LunaStudio.Data.PortRef            (InPortRef)
import           LunaStudio.Data.ScreenPosition     (ScreenPosition, x)
import           NodeEditor.Action.Basic            (localSetPortDefault)
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.State.Action     (beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import           NodeEditor.Action.State.NodeEditor (getPortDefault)
import           NodeEditor.State.Action            (Action (begin, continue, end, update), InitValue (Continous, Discrete),
                                                     SliderDrag (SliderDrag), sliderDragAction, sliderDragInitValue, sliderDragPortRef,
                                                     sliderDragStartTime)
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


startMoveSlider :: InPortRef -> InitValue -> Command State ()
startMoveSlider portRef initVal = do
    time <- liftIO Clock.getCurrentTime
    begin $ SliderDrag portRef time initVal
    JS.lockCursor

moveSlider :: ScreenPosition -> SliderDrag -> Command State ()
moveSlider movement state = do
    currentTime <- liftIO Clock.getCurrentTime
    let portRef = state ^. sliderDragPortRef
        newValue = newSliderValue movement currentTime state
    begin $ state & sliderDragStartTime .~ currentTime
                  & sliderDragInitValue .~ newValue
    void $ localSetPortDefault portRef $ toPortValue newValue

stopMoveSlider :: ScreenPosition -> SliderDrag -> Command State ()
stopMoveSlider _currentPostion state = do
    JS.unlockCursor
    -- currentTime <- liftIO Clock.getCurrentTime
    let portRef = state ^. sliderDragPortRef
        newValue = state ^. sliderDragInitValue
    Batch.setPortDefault portRef $ toPortValue newValue
    removeActionFromState sliderDragAction

sign :: Double -> Double
sign a = if a < 0 then -1 else 1

newSliderValue :: ScreenPosition -> UTCTime -> SliderDrag -> InitValue
newSliderValue currentPostion currentTime slider =
    let dx   = currentPostion ^. x
        dt   = fromRational $ toRational $ Clock.diffUTCTime currentTime (slider ^. sliderDragStartTime)
        v    = dx / dt -- 100 - 4 000
        max' = 100 :: Double
        nv   = v / max' :: Double
        f :: Double -> Double
        f x' = x' + x'' * nv where
          x'' = sign x' + (x' ** 0.5)
        -- f x = x + nv * x' * a where
        -- a = 1 :: Double
        -- b = 0.9  :: Double
        -- c = 0.9  :: Double
        -- f val
        --   | val == 0 && v <  0 =     - a * (abs v ** b)
        --   | val == 0           =       a * (v ** b)
        --   |             v == 0 = val
        --   |             v <  0 = val - a * (abs (val * v) ** b)
        --   | otherwise          = val + a * ((val * v) ** b)
        --
    in case slider ^. sliderDragInitValue of
          Continous val -> Continous $ f val
          Discrete  val -> Discrete  $ round $ f $ fromIntegral val

toPortValue :: InitValue -> PortDefault
toPortValue = Constant . \case
    Continous val -> DoubleValue val
    Discrete  val -> IntValue    val
