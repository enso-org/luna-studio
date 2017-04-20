module NodeEditor.Action.Basic.EnterBreadcrumb where

import           Empire.API.Data.Breadcrumb                  (Breadcrumb, BreadcrumbItem (Lambda), items)
import           Empire.API.Data.GraphLocation               (breadcrumb)
import           NodeEditor.Action.Basic.ProjectManager     (navigateToGraph)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Batch.Workspace                 (currentLocation)
import           Common.Prelude
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, canEnter, nodeId)
import           NodeEditor.State.Global                    (State, workspace)


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
