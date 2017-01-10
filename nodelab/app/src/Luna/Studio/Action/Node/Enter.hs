module Luna.Studio.Action.Node.Enter
    ( enter
    , exit
    , tryEnter
    ) where

import           Empire.API.Data.Breadcrumb        (BreadcrumbItem)
import qualified Empire.API.Data.Breadcrumb        as Breadcrumb
import qualified Empire.API.Data.GraphLocation     as GraphLocation
import           Empire.API.Data.Node              (Node)
import qualified Empire.API.Data.Node              as Node
import           Luna.Studio.Action.Command        (Command)
import           Luna.Studio.Action.ProjectManager as ProjectManager
import qualified Luna.Studio.Batch.Workspace       as Workspace
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global          (State)
import qualified Luna.Studio.State.Global          as Global


tryEnter :: Node -> Command State ()
tryEnter node = when (node ^. Node.canEnter) $
    enter $ Breadcrumb.Lambda $ node ^. Node.nodeId

enter :: BreadcrumbItem -> Command State ()
enter item = do
    location <- use $ Global.workspace . Workspace.currentLocation
    let newLocation = location & GraphLocation.breadcrumb . Breadcrumb.items %~ (++ [item])
    ProjectManager.navigateToGraph newLocation

exit :: Command State ()
exit = do
    location <- use $ Global.workspace . Workspace.currentLocation
    case location ^. GraphLocation.breadcrumb . Breadcrumb.items of
        (_:t) -> ProjectManager.navigateToGraph $ location & GraphLocation.breadcrumb . Breadcrumb.items .~ t
        [] -> return ()
