module Reactive.Commands.Node
    ( enter
    , exit
    , tryEnter
    , rename
    ) where

import           Utils.PreludePlus

import qualified Batch.Workspace                  as Workspace
import           Empire.API.Data.Breadcrumb       (BreadcrumbItem (..))
import qualified Empire.API.Data.Breadcrumb       as Breadcrumb
import qualified Empire.API.Data.GraphLocation    as GraphLocation
import           Empire.API.Data.Node             (Node, NodeId)
import qualified Empire.API.Data.Node             as Node
import qualified Object.Widget.Node               as Model
import qualified React.Store                      as Store
import           Reactive.Commands.Command        (Command)
import           Reactive.Commands.ProjectManager as ProjectManager
import           Reactive.State.Global            (State)
import qualified Reactive.State.Global            as Global
import qualified Reactive.State.Graph             as Graph



tryEnter :: Node -> Command State ()
tryEnter node = when (node ^. Node.canEnter) $
    enter $ Breadcrumb.Lambda $ node ^. Node.nodeId

enter :: BreadcrumbItem -> Command State ()
enter item = do
    location <- use $ Global.workspace . Workspace.currentLocation
    let newLocation = location & GraphLocation.breadcrumb . Breadcrumb.items %~ (item:)
    ProjectManager.navigateToGraph newLocation

exit :: Command State ()
exit = do
    location <- use $ Global.workspace . Workspace.currentLocation
    case location ^. GraphLocation.breadcrumb . Breadcrumb.items of
        (_:t) -> ProjectManager.navigateToGraph $ location & GraphLocation.breadcrumb . Breadcrumb.items .~ t
        [] -> return ()


rename :: NodeId -> Text -> Command Global.State ()
rename nodeId name = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.name .= name
    Global.withNode nodeId $
        mapM_ $ Store.modify_ (Model.name .~ name)
