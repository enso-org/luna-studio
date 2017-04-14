module Luna.Studio.Action.State.Model.Sidebar where

import           Control.Monad.Trans.Maybe         (MaybeT (MaybeT), runMaybeT)
import           Data.Position                     (Position, fromDoubles, fromTuple, move, vector)
import           Data.ScreenPosition               (ScreenPosition (ScreenPosition), toTuple, x)
import qualified Data.ScreenPosition               as SP
import           Data.Size                         (Size)
import           Data.Vector                       (Vector2 (Vector2))
import           JS.Scene                          (inputSidebarPosition, inputSidebarSize, outputSidebarPosition)
import           Luna.Studio.Action.Command        (Command)
import           Luna.Studio.Action.State.Scene    (getInputSidebar, getInputSidebarPosition, getOutputSidebar, translateToWorkspace)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Constants (gridSize)
import           Luna.Studio.React.Model.Port      (InPort, InPortId, OutPort, OutPortId, getPortNumber, getPositionInSidebar, isSelf,
                                                    portId)
import           Luna.Studio.State.Global          (State)


-- WARNING: Since getInputSidebar and getOutputSidebar can change scene redrawConnectionForSidebarNodes may be needed after use of those function

getMousePositionInInputSidebar :: ScreenPosition -> Command State (Maybe Position)
getMousePositionInInputSidebar mousePos = runMaybeT $ do
    pos <- MaybeT getInputSidebarPosition
    let (posX, posY)     = toTuple pos
        (mouseX, mouseY) = toTuple mousePos
    return $ fromTuple (mouseX - posX, mouseY - posY)

getPortPositionInInputSidebar :: Size -> OutPortId -> Position
getPortPositionInInputSidebar sidebarSize pid = fromDoubles posX posY where
    portNum = getPortNumber pid
    posX    = sidebarSize ^. x
    posY    = (fromIntegral portNum) * gridSize

getPortPositionInOutputSidebar :: InPortId -> Position
getPortPositionInOutputSidebar pid = fromDoubles 0 posY where
    portNum = getPortNumber pid
    posY    = (fromIntegral $ if isSelf pid then 0 else portNum) * gridSize

getInputSidebarPortPosition :: OutPort -> Command State (Maybe Position)
getInputSidebarPortPosition p = do
    let pid = p ^. portId
    mayInputSidebar <- getInputSidebar
    flip (maybe (return Nothing)) mayInputSidebar $
        \inputSidebar -> do
            let shift = inputSidebar ^. inputSidebarPosition . vector + Vector2 0 gridSize
                siz   = inputSidebar ^. inputSidebarSize
                pos   = ScreenPosition . view SP.vector . move shift $
                    maybe (getPortPositionInInputSidebar siz pid) id (getPositionInSidebar p)
            Just <$> translateToWorkspace pos

getOutputSidebarPortPosition :: InPort -> Command State (Maybe Position)
getOutputSidebarPortPosition p = do
    let pid = p ^. portId
    mayOutputSidebar <- getOutputSidebar
    flip (maybe (return Nothing)) mayOutputSidebar $
        \outputSidebar -> do
            let shift = outputSidebar ^. outputSidebarPosition . vector + Vector2 0 gridSize
                pos   = ScreenPosition . view SP.vector . move shift $
                    maybe (getPortPositionInOutputSidebar pid) id (getPositionInSidebar p)
            Just <$> translateToWorkspace pos
