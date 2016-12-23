{-# LANGUAGE JavaScriptFFI #-}
module Luna.Studio.Commands.PortControl (
    startMoveSlider,
    moveSlider,
    stopMoveSlider
) where

import qualified Empire.API.Data.DefaultValue as DefaultValue
import qualified Empire.API.Data.Port         as PortAPI
import           Empire.API.Data.PortRef      (AnyPortRef)
import qualified Empire.API.Data.PortRef      as PortRef
import qualified Luna.Studio.Commands.Batch   as Batch
import           Luna.Studio.Commands.Command (Command)
import           Luna.Studio.Data.Vector      (Position, x)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Store      as Store
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.Slider     as Slider
import qualified Object.Widget.Node           as Node
import qualified Object.Widget.Port           as Port


startMoveSlider :: AnyPortRef -> Position -> Slider.InitValue -> Command State ()
startMoveSlider portRef initPos startVal = do
    Global.slider ?= Slider.State portRef initPos startVal
    liftIO setMovingCursor

moveSlider :: Position -> Command State ()
moveSlider currentPostion = do
    maySlider <- use Global.slider
    withJust maySlider $ \slider -> do
        let defaultValue = newDefaultValue currentPostion slider
            portRef = slider ^. Slider.portRef
        Global.withNode (portRef ^. PortRef.nodeId) $ mapM_ $ Store.modify_ $
            Node.ports . ix portRef . Port.state .~ PortAPI.WithDefault defaultValue

stopMoveSlider :: Position -> Command State ()
stopMoveSlider currentPostion = do
    maySlider <- use Global.slider
    Global.slider .= Nothing
    withJust maySlider $ \slider -> do
        liftIO setDefaultCursor
        let defaultValue = newDefaultValue currentPostion slider
            portRef = slider ^. Slider.portRef
        Batch.setDefaultValue portRef defaultValue

newDefaultValue :: Position -> Slider.State -> DefaultValue.PortDefault
newDefaultValue currentPostion slider =
    let delta = currentPostion ^. x - slider ^. Slider.startPos . x
    in DefaultValue.Constant $ case slider ^. Slider.initValue of
          Slider.Discrete  val -> DefaultValue.IntValue    $ val + round delta
          Slider.Continous val -> DefaultValue.DoubleValue $ val + delta

foreign import javascript safe "document.body.style.cursor = \"auto\";" setDefaultCursor :: IO ()
foreign import javascript safe "document.body.style.cursor = \"col-resize\";" setMovingCursor :: IO ()
