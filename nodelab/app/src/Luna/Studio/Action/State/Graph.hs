module Luna.Studio.Action.State.Graph where

import qualified Data.List                     as List
import           Empire.API.Data.Breadcrumb    (Breadcrumb (Breadcrumb), items)
import           Empire.API.Data.GraphLocation (GraphLocation, breadcrumb)
import           Empire.API.Data.NodeLoc       (NodePath)
import           Luna.Studio.Action.Command    (Command)
import           Luna.Studio.Batch.Workspace   (currentLocation, isGraphLoaded)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global      (State, workspace)


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (workspace . currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ workspace . isGraphLoaded
    return $ icl && igl

inCurrentLocation :: GraphLocation -> (NodePath -> Command State ()) -> Command State ()
inCurrentLocation location action =
    whenM (use $ workspace . isGraphLoaded) $ do
        currentBc <- use (workspace . currentLocation . breadcrumb . items)
        withJust (List.stripPrefix currentBc $ location ^. breadcrumb . items) $
            action . convert . Breadcrumb
