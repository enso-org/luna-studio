module Node.Editor.Action.Basic.EnterBreadcrumb where

import           Empire.API.Data.Breadcrumb                  (Breadcrumb, BreadcrumbItem (Lambda), items)
import           Empire.API.Data.GraphLocation               (breadcrumb)
import           Node.Editor.Action.Basic.ProjectManager     (navigateToGraph)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Batch.Workspace                 (currentLocation)
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (ExpressionNode, canEnter, nodeId)
import           Node.Editor.State.Global                    (State, workspace)


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
