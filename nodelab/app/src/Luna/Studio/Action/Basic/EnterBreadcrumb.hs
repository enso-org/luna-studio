module Luna.Studio.Action.Basic.EnterBreadcrumb where

import           Empire.API.Data.Breadcrumb                  (Breadcrumb, BreadcrumbItem (Lambda), items)
import           Empire.API.Data.GraphLocation               (breadcrumb)
import           Luna.Studio.Action.Basic.ProjectManager     (navigateToGraph)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Batch.Workspace                 (currentLocation)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, canEnter, nodeId)
import           Luna.Studio.State.Global                    (State, workspace)


enterBreadcrumb :: BreadcrumbItem -> Command State ()
enterBreadcrumb item = do
    location <- use $ workspace . currentLocation
    navigateToGraph $ location & breadcrumb . items %~ (++ [item])

enterBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command State ()
enterBreadcrumbs newBc = do
    location <- use $ workspace . currentLocation
    navigateToGraph $ location & breadcrumb .~ newBc

exitBreadcrumb :: Command State ()
exitBreadcrumb = do
    location <- use $ workspace . currentLocation
    case location ^. breadcrumb . items of
        (_:t) -> navigateToGraph $ location & breadcrumb . items .~ t
        []    -> return ()

enterNode :: ExpressionNode -> Command State ()
enterNode node = when (node ^. canEnter) $ enterBreadcrumb $ Lambda (node ^. nodeId)
