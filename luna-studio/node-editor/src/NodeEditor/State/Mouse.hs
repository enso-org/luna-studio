module NodeEditor.State.Mouse where

import           Common.Action.Command          (Command)
import           Common.Prelude
import           LunaStudio.Data.Position       (Position)
import           LunaStudio.Data.ScreenPosition (ScreenPosition, fromDoubles)
import           NodeEditor.Action.State.Scene  (getWorkspacePosition, translateToWorkspace)
import           NodeEditor.State.Global        (State)
import           React.Flux                     (MouseEvent, mousePageX, mousePageY)


workspacePosition :: MouseEvent -> Command State Position
workspacePosition = translateToWorkspace <=< mousePosition

mousePosition :: MouseEvent -> Command State ScreenPosition
mousePosition e = getWorkspacePosition >>= return . maybe pagePos (\workspacePos -> pagePos - workspacePos) where
    pagePos = fromDoubles (fromIntegral $ mousePageX e) (fromIntegral $ mousePageY e)
