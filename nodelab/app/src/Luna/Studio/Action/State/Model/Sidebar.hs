module Luna.Studio.Action.State.Model.Sidebar where

import           Control.Monad.Trans.Maybe             (MaybeT (MaybeT), runMaybeT)
import           Data.Position                         (Position, fromDoubles, fromTuple, move, vector)
import           Data.ScreenPosition                   (ScreenPosition (ScreenPosition), toTuple, x)
import qualified Data.ScreenPosition                   as SP
import           Data.Size                             (Size)
import           JS.Scene                              (inputSidebarPosition, inputSidebarSize, outputSidebarPosition)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.State.Scene        (getInputSidebar, getInputSidebarPosition, getOutputSidebar, translateToWorkspace)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Constants     (gridSize)
import           Luna.Studio.React.Model.Node.EdgeNode (EdgeType (InputEdge, OutputEdge))
import           Luna.Studio.React.Model.Port          (PortId, getPortNumber, isSelf)
import           Luna.Studio.State.Global              (State)


-- WARNING: Since getInputSidebar and getOutputSidebar can change scene redrawConnectionForEdgeNodes may be needed after use of those function

getMousePositionInSidebar :: ScreenPosition -> EdgeType -> Command State (Maybe Position)
getMousePositionInSidebar mousePos type' = case type' of
        InputEdge -> runMaybeT $ do
            pos <- MaybeT getInputSidebarPosition
            let (posX, posY)     = toTuple pos
                (mouseX, mouseY) = toTuple mousePos
            return $ fromTuple (mouseX - posX, mouseY - posY)
        OutputEdge -> $notImplemented

getPortPositionInInputSidebar :: Size -> PortId -> Position
getPortPositionInInputSidebar sidebarSize pid = fromDoubles posX posY where
    portNum = getPortNumber pid
    posX    = sidebarSize ^. x
    posY    = (fromIntegral (portNum + 1)) * gridSize

getPortPositionInOutputSidebar :: PortId -> Position
getPortPositionInOutputSidebar pid = fromDoubles 0 posY where
    portNum = getPortNumber pid
    posY    = (fromIntegral $ if isSelf pid then 0 else portNum + 1) * gridSize

getInputEdgePortPosition :: PortId -> Command State (Maybe Position)
getInputEdgePortPosition pid = do
    mayInputSidebar <- getInputSidebar
    flip (maybe (return Nothing)) mayInputSidebar $
        \inputSidebar -> do
            let shift = inputSidebar ^. inputSidebarPosition . vector
                siz   = inputSidebar ^. inputSidebarSize
                pos   = ScreenPosition . view SP.vector . move shift $ getPortPositionInInputSidebar siz pid
            Just <$> translateToWorkspace pos

getOutputEdgePortPosition :: PortId -> Command State (Maybe Position)
getOutputEdgePortPosition pid = do
    mayOutputSidebar <- getOutputSidebar
    flip (maybe (return Nothing)) mayOutputSidebar $
        \outputSidebar -> do
            let shift = outputSidebar ^. outputSidebarPosition . vector
                pos   = ScreenPosition . view SP.vector . move shift $ getPortPositionInOutputSidebar pid
            Just <$> translateToWorkspace pos
