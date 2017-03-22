module Luna.Studio.Action.State.Model.Port where

import           Data.Position                     (Position)
import           Data.ScreenPosition               (fromDoubles, x, y)
import           Empire.API.Data.Port              (PortId, getPortNumber, isSelf)
import           JS.Scene                          (inputSidebarPosition, inputSidebarSize, outputSidebarPosition, outputSidebarSize)
import           Luna.Studio.Action.Command        (Command)
import           Luna.Studio.Action.State.Scene    (translateToWorkspace)
import           Luna.Studio.Action.State.Scene    (getInputSidebar, getOutputSidebar)
import           Luna.Studio.Data.Angle            (Angle)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Constants (gridSize, nodeRadius)
import           Luna.Studio.State.Global          (State)


-- WARNING: Since getInputSidebar and getOutputSidebar can change scene redrawConnectionForEdgeNodes may be needed after use of those function

getInputEdgePortPosition :: PortId -> Command State (Maybe Position)
getInputEdgePortPosition pid = do
    mayInputSidebar <- getInputSidebar
    flip (maybe (return Nothing)) mayInputSidebar $ \inputSidebar -> do
        let portNum = getPortNumber pid
            pos     = inputSidebar ^. inputSidebarPosition
            siz     = inputSidebar ^. inputSidebarSize
            posX    = pos ^. x + siz ^. x
            posY    = pos ^. y + (fromIntegral (portNum + 1)) * gridSize
        (fmap Just) . translateToWorkspace $ fromDoubles posX posY

getOutputEdgePortPosition :: PortId -> Command State (Maybe Position)
getOutputEdgePortPosition pid = do
    mayOutputSidebar <- getOutputSidebar
    flip (maybe (return Nothing)) mayOutputSidebar $ \outputSidebar -> do
        let portNum = getPortNumber pid
            pos     = outputSidebar ^. outputSidebarPosition
            siz     = outputSidebar ^. outputSidebarSize
            posX    = pos ^. x
            posY    = (fromIntegral $ if isSelf pid then 0 else  portNum + 1) * gridSize + pos ^. y
        (fmap Just) . translateToWorkspace $ fromDoubles posX posY


portGap :: Double -> Angle
portGap r = 0.2 * nodeRadius / r -- to avoid gap narrowing

portAngle :: Int -> Angle
portAngle numOfPorts = pi / fromIntegral numOfPorts

portAngleStart :: Bool -> Int -> Int -> Double -> Angle
portAngleStart isShape num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = if isShape then (portGap r)/2 else 0
        t      = portAngle numOfPorts
    in  pi - number * t + gap

portAngleStop :: Bool -> Int -> Int -> Double -> Angle
portAngleStop isShape num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = if isShape then (portGap r)/2 else 0
        t      = portAngle numOfPorts
    in  pi - number * t + t - gap
