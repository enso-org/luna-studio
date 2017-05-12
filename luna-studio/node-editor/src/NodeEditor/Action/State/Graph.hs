module NodeEditor.Action.State.Graph where

import           Common.Prelude
import qualified Data.List                          as List
import           LunaStudio.Data.Breadcrumb         (Breadcrumb (Breadcrumb), items)
import           LunaStudio.Data.GraphLocation      (GraphLocation, breadcrumb)
import           LunaStudio.Data.NodeLoc            (NodePath)
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (isGraphLoaded)
import           NodeEditor.Batch.Workspace         (currentLocation)
import           NodeEditor.State.Global            (State, workspace)


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = use workspace >>= return . \case
    Just w  -> w ^. currentLocation == location
    Nothing -> False

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- isGraphLoaded
    return $ icl && igl

inCurrentLocation :: GraphLocation -> (NodePath -> Command State ()) -> Command State ()
inCurrentLocation location action =
    whenM isGraphLoaded $ do
        currentBc <- fromMaybe def <$> preuse (workspace . _Just . currentLocation . breadcrumb . items)
        withJust (List.stripPrefix currentBc $ location ^. breadcrumb . items) $
            action . convert . Breadcrumb
