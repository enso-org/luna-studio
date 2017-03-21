module Luna.Studio.Action.State.Graph where

import           Empire.API.Data.GraphLocation (GraphLocation)
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
