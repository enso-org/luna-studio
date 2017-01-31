{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.PortControl
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    , setPortDefault
    ) where

import qualified Data.Map                     as Map
import           Data.Position                (Position, x)
import qualified Empire.API.Data.DefaultValue as DefaultValue
import qualified Empire.API.Data.Port         as PortAPI
import           Empire.API.Data.PortRef      (AnyPortRef)
import qualified Empire.API.Data.PortRef      as PortRef
import qualified Luna.Studio.Action.Batch     as Batch
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node as Node
import qualified Luna.Studio.React.Model.Port as Port
import           Luna.Studio.State.Action     (Action (begin, continue, end, update), InitValue, SliderDrag (SliderDrag), sliderDragAction)
import qualified Luna.Studio.State.Action     as Action
import           Luna.Studio.State.Global     (State, beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import qualified Luna.Studio.State.Global     as Global

instance Action (Command State) SliderDrag where
    begin    = beginActionWithKey    sliderDragAction
    continue = continueActionWithKey sliderDragAction
    update   = updateActionWithKey   sliderDragAction
    end a = do
        removeActionFromState sliderDragAction
        let portRef = a ^. Action.sliderDragPortRef
        mayDefValue <- getPortDefault portRef
        withJust mayDefValue $ Batch.setDefaultValue portRef
        liftIO setDefaultCursor


setPortDefault :: AnyPortRef -> DefaultValue.PortDefault -> Command State ()
setPortDefault portRef defaultValue = do
    Global.modifyNode (portRef ^. PortRef.nodeId) $
        Node.ports . ix portRef . Port.state .= PortAPI.WithDefault defaultValue

getPortDefault :: AnyPortRef -> Command State (Maybe DefaultValue.PortDefault)
getPortDefault portRef = do
    mayNode <- Global.getNode (portRef ^. PortRef.nodeId)
    let mayPort = mayNode >>= (Map.lookup portRef . view Node.ports)
    return $ mayPort ^? _Just . Port.state . PortAPI._WithDefault

startMoveSlider :: AnyPortRef -> Position -> InitValue -> Command State ()
startMoveSlider portRef initPos startVal = do
    begin $ SliderDrag portRef initPos startVal
    liftIO setMovingCursor

moveSlider :: Position -> SliderDrag -> Command State ()
moveSlider currentPostion state = do
    let defaultValue = newDefaultValue currentPostion state
        portRef = state ^. Action.sliderDragPortRef
    setPortDefault portRef defaultValue

stopMoveSlider :: Position -> SliderDrag -> Command State ()
stopMoveSlider currentPostion state = do
    removeActionFromState sliderDragAction
    liftIO setDefaultCursor
    let defaultValue = newDefaultValue currentPostion state
        portRef = state ^. Action.sliderDragPortRef
    Batch.setDefaultValue portRef defaultValue

newDefaultValue :: Position -> SliderDrag -> DefaultValue.PortDefault
newDefaultValue currentPostion slider =
    let delta = currentPostion ^. x - slider ^. Action.sliderDragStartPos . x
    in DefaultValue.Constant $ case slider ^. Action.sliderDragInitValue of
          Action.Discrete  val -> DefaultValue.IntValue    $ val + round delta
          Action.Continous val -> DefaultValue.DoubleValue $ val + delta

foreign import javascript safe "document.body.style.cursor = \"auto\";" setDefaultCursor :: IO ()
foreign import javascript safe "document.body.style.cursor = \"col-resize\";" setMovingCursor :: IO ()
