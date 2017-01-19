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
import           Luna.Studio.State.Action     (Action (begin, continue, end, update), InitValue, SliderDrag (SliderDrag), fromSomeAction,
                                               sliderDragAction, someAction)
import qualified Luna.Studio.State.Action     as Action
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global

instance Action (Command State) SliderDrag where
    begin a = do
        currentOverlappingActions <- Global.getCurrentOverlappingActions sliderDragAction
        mapM_ end currentOverlappingActions
        update a
    continue run = do
        maySomeAction <- preuse $ Global.currentActions . ix sliderDragAction
        withJust (join $ fromSomeAction <$> maySomeAction) $ run
    update a = Global.currentActions . at sliderDragAction ?= someAction a
    end a = do
        let portRef = a ^. Action.sliderDragPortRef
        mayDefValue <- getPortDefault portRef
        withJust mayDefValue $ Batch.setDefaultValue portRef
        liftIO setDefaultCursor
        Global.currentActions %= Map.delete sliderDragAction



setPortDefault :: AnyPortRef -> DefaultValue.PortDefault -> Command State ()
setPortDefault portRef defaultValue = do
    Global.modifyNode (portRef ^. PortRef.nodeId) $
        Node.ports . ix portRef . Port.state .= PortAPI.WithDefault defaultValue

getPortDefault :: AnyPortRef -> Command State (Maybe DefaultValue.PortDefault)
getPortDefault portRef = do
    mayNode <- Global.getNode (portRef ^. PortRef.nodeId)
    case mayNode of
        Just node -> do
            let mayPort = Map.lookup portRef (node ^. Node.ports)
            case (view Port.state <$> mayPort) of
                Just (PortAPI.WithDefault portDef) -> return $ Just portDef
                _ -> return Nothing
        Nothing -> return Nothing

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
    liftIO setDefaultCursor
    let defaultValue = newDefaultValue currentPostion state
        portRef = state ^. Action.sliderDragPortRef
    Batch.setDefaultValue portRef defaultValue
    Global.currentActions %= Map.delete sliderDragAction

newDefaultValue :: Position -> SliderDrag -> DefaultValue.PortDefault
newDefaultValue currentPostion slider =
    let delta = currentPostion ^. x - slider ^. Action.sliderDragStartPos . x
    in DefaultValue.Constant $ case slider ^. Action.sliderDragInitValue of
          Action.Discrete  val -> DefaultValue.IntValue    $ val + round delta
          Action.Continous val -> DefaultValue.DoubleValue $ val + delta

foreign import javascript safe "document.body.style.cursor = \"auto\";" setDefaultCursor :: IO ()
foreign import javascript safe "document.body.style.cursor = \"col-resize\";" setMovingCursor :: IO ()
