{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.Port.Control
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    , editTextPortControl
    , acceptEditTextPortControl
    , unfocusEditTextPortControl
    , rollbackEditTextPortControl
    ) where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           Data.Time.Clock                    (UTCTime)
import qualified Data.Time.Clock                    as Clock
import           JS.Scene                           (appId)
import qualified JS.UI                              as JS
import           LunaStudio.Data.PortDefault        (PortDefault (Constant), PortValue (IntValue, RealValue, TextValue))
import           LunaStudio.Data.PortRef            (InPortRef)
import           LunaStudio.Data.ScreenPosition     (ScreenPosition, x)
import           NodeEditor.Action.Basic            (localSetPortDefault, setPortDefault)
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.State.Action     (beginActionWithKey, checkAction, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import           NodeEditor.Action.State.NodeEditor (getPortDefault, modifyNodeEditor)
import           NodeEditor.Data.Slider             (InitValue (Continous, Discrete))
import qualified NodeEditor.React.Model.NodeEditor  as NE
import           NodeEditor.State.Action            (Action (begin, continue, end, update), SliderDrag (SliderDrag),
                                                     TextPortControlEdit (TextPortControlEdit), sliderDragAction, sliderDragInitValue,
                                                     sliderDragPortRef, sliderDragStartTime, textPortControlEditAction,
                                                     textPortControlEditPortRef, textPortControlEditValue)
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

instance Action (Command State) TextPortControlEdit where
    begin action = do
        beginActionWithKey textPortControlEditAction action
        modifyNodeEditor $ NE.textControlEditedPortRef ?= action ^. textPortControlEditPortRef
    continue   = continueActionWithKey textPortControlEditAction
    update     = updateActionWithKey   textPortControlEditAction
    end action = do
        let portRef       = action ^. textPortControlEditPortRef
            portDef       = Constant . TextValue . convert $ action ^. textPortControlEditValue
        modifyNodeEditor $ NE.textControlEditedPortRef .= def
        setPortDefault portRef portDef
        removeActionFromState textPortControlEditAction

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

newSliderValue :: ScreenPosition -> UTCTime -> SliderDrag -> InitValue
newSliderValue currentPostion currentTime slider =
    let dx   = currentPostion ^. x
        f :: Double -> Double
        f x' = x' + dx
    in case slider ^. sliderDragInitValue of
          Continous val -> Continous $ f val
          Discrete  val -> Discrete  $ round $ f $ fromIntegral val

toPortValue :: InitValue -> PortDefault
toPortValue = Constant . \case
    Continous val -> RealValue val
    Discrete  val -> IntValue  val


editTextPortControl :: InPortRef -> Text -> Command State ()
editTextPortControl portRef val = do
    mayCurrentPortRef <- view textPortControlEditPortRef `fmap2` checkAction textPortControlEditAction
    let updateAction  = update $ TextPortControlEdit portRef val
        replaceAction = do
            continue (end :: TextPortControlEdit -> Command State ())
            begin $ TextPortControlEdit portRef val
    if (mayCurrentPortRef /= Just portRef) then replaceAction else updateAction

acceptEditTextPortControl :: TextPortControlEdit -> Command State ()
acceptEditTextPortControl = end

unfocusEditTextPortControl :: Command State ()
unfocusEditTextPortControl = JS.focus appId

rollbackEditTextPortControl :: TextPortControlEdit -> Command State ()
rollbackEditTextPortControl _ = do
    modifyNodeEditor $ NE.textControlEditedPortRef .= def
    removeActionFromState textPortControlEditAction
