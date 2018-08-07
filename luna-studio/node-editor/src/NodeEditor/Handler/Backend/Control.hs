module NodeEditor.Handler.Backend.Control
    ( handle
    ) where

import           Common.Action.Command         (Command)
import           Common.Prelude
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified NodeEditor.Action.Batch       as Batch
import           NodeEditor.Action.State.App   (getWorkspace)
import           NodeEditor.Batch.Workspace    (currentLocation)
import qualified NodeEditor.Event.Batch        as Batch
import           NodeEditor.Event.Event        (Event (Batch))
import           NodeEditor.State.Global       (State)


handle :: Event -> Maybe (Command State ())
handle (Batch (Batch.EmpireStarted _)) = Just $ do
    workspace <- getWorkspace
    withJust workspace $ \ws -> do
        let gl = ws ^. currentLocation
        Batch.openFile $ gl ^. GraphLocation.filePath
        Batch.getProgram Nothing True
handle _ = Nothing
