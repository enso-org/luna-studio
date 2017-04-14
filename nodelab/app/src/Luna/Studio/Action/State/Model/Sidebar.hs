module Luna.Studio.Action.State.Model.Sidebar where

import           Data.Position                     (Position, move, vector)
import           Data.ScreenPosition               (ScreenPosition (ScreenPosition))
import qualified Data.ScreenPosition               as SP
import           Data.Vector                       (Vector2 (Vector2))
import           Luna.Studio.Action.Command        (Command)
import           Luna.Studio.Action.State.Scene    (getInputSidebar, getOutputSidebar, translateToWorkspace)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Constants (gridSize)
import           Luna.Studio.React.Model.Port      (InPort, OutPort, getPositionInSidebar, portId)
import           Luna.Studio.React.Model.Sidebar   (inputSidebarPosition, inputSidebarSize, outputSidebarPosition)
import           Luna.Studio.React.Model.Sidebar   (portPositionInInputSidebar, portPositionInOutputSidebar)
import           Luna.Studio.State.Global          (State)


-- WARNING: Since getInputSidebar and getOutputSidebar can change scene redrawConnectionForSidebarNodes may be needed after use of those function

getInputSidebarPortPosition :: OutPort -> Command State (Maybe Position)
getInputSidebarPortPosition p = do
    let pid = p ^. portId
    mayInputSidebar <- getInputSidebar
    flip (maybe (return Nothing)) mayInputSidebar $
        \inputSidebar -> do
            let shift = inputSidebar ^. inputSidebarPosition . vector + Vector2 0 gridSize
                siz   = inputSidebar ^. inputSidebarSize
                pos   = ScreenPosition . view SP.vector . move shift $
                    maybe (portPositionInInputSidebar siz pid) id (getPositionInSidebar p)
            Just <$> translateToWorkspace pos

getOutputSidebarPortPosition :: InPort -> Command State (Maybe Position)
getOutputSidebarPortPosition p = do
    let pid = p ^. portId
    mayOutputSidebar <- getOutputSidebar
    flip (maybe (return Nothing)) mayOutputSidebar $
        \outputSidebar -> do
            let shift = outputSidebar ^. outputSidebarPosition . vector + Vector2 0 gridSize
                pos   = ScreenPosition . view SP.vector . move shift $
                    maybe (portPositionInOutputSidebar pid) id (getPositionInSidebar p)
            Just <$> translateToWorkspace pos
