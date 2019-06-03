module NodeEditor.Action.Basic.MoveProject where

import           Common.Action.Command                  (Command)
import           Common.Prelude
import           LunaStudio.Data.GraphLocation          (filePath)
import           NodeEditor.Action.State.App            (modifyApp)
import           NodeEditor.Batch.Workspace             (currentLocation)
import           NodeEditor.React.Model.App             (workspace)
import           NodeEditor.State.Global                (State)


localMoveProject :: FilePath -> FilePath -> Command State ()
localMoveProject old new = pure () -- modifyApp $ do
    {-withJustM (use workspace) $ \workspace' -> do-}
        {-let oldFilePath = workspace' ^. currentLocation . filePath-}
        {-withJust (stripPrefix old oldFilePath) $ \fileName ->-}
            {-workspace . _Just . currentLocation . filePath .= new <> fileName-}
