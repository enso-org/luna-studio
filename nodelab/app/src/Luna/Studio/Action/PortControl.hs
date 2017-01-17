{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.PortControl
    ( startMoveSlider
    , moveSlider
    , stopMoveSlider
    , setPortDefault
    ) where

import qualified Empire.API.Data.DefaultValue     as DefaultValue
import qualified Empire.API.Data.Port             as PortAPI
import           Empire.API.Data.PortRef          (AnyPortRef)
import qualified Empire.API.Data.PortRef          as PortRef
import qualified Luna.Studio.Action.Batch         as Batch
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Data.Vector          (Position, x)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node     as Node
import qualified Luna.Studio.React.Model.Port     as Port
import           Luna.Studio.State.Action         (Action (Slider))
import           Luna.Studio.State.Global         (State)
import qualified Luna.Studio.State.Global         as Global
import qualified Luna.Studio.State.Slider         as Slider
import           Luna.Studio.State.StatefulAction (StatefulAction (exit, matchState, pack, start))

instance StatefulAction Slider.State where
    matchState (Slider state) = Just state
    matchState _ = Nothing
    pack = Slider
    exit _ = Global.performedAction .= Nothing


setPortDefault :: AnyPortRef -> DefaultValue.PortDefault -> Command State ()
setPortDefault portRef defaultValue = do
    Global.modifyNode (portRef ^. PortRef.nodeId) $
        Node.ports . ix portRef . Port.state .= PortAPI.WithDefault defaultValue

startMoveSlider :: AnyPortRef -> Position -> Slider.InitValue -> Command State ()
startMoveSlider portRef initPos startVal = do
    start $ Slider.State portRef initPos startVal
    liftIO setMovingCursor

moveSlider :: Position -> Slider.State -> Command State ()
moveSlider currentPostion state = do
    let defaultValue = newDefaultValue currentPostion state
        portRef = state ^. Slider.portRef
    setPortDefault portRef defaultValue

stopMoveSlider :: Position -> Slider.State -> Command State ()
stopMoveSlider currentPostion state = do
    liftIO setDefaultCursor
    let defaultValue = newDefaultValue currentPostion state
        portRef = state ^. Slider.portRef
    Batch.setDefaultValue portRef defaultValue
    mayPerformedAction <- use $ Global.performedAction
    Global.performedAction .= Nothing

newDefaultValue :: Position -> Slider.State -> DefaultValue.PortDefault
newDefaultValue currentPostion slider =
    let delta = currentPostion ^. x - slider ^. Slider.startPos . x
    in DefaultValue.Constant $ case slider ^. Slider.initValue of
          Slider.Discrete  val -> DefaultValue.IntValue    $ val + round delta
          Slider.Continous val -> DefaultValue.DoubleValue $ val + delta

foreign import javascript safe "document.body.style.cursor = \"auto\";" setDefaultCursor :: IO ()
foreign import javascript safe "document.body.style.cursor = \"col-resize\";" setMovingCursor :: IO ()
