module NodeEditor.Action.Basic.EnterBreadcrumb where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           LunaStudio.Data.Breadcrumb                 (Breadcrumb, BreadcrumbItem (Definition, Lambda), items)
import           LunaStudio.Data.GraphLocation              (breadcrumb)
import           NodeEditor.Action.Basic.ProjectManager     (navigateToGraph)
import           NodeEditor.Batch.Workspace                 (currentLocation)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, canEnter, isDefinition, nodeId)
import           NodeEditor.State.Global                    (State, workspace)


enterBreadcrumb :: BreadcrumbItem -> Command State ()
enterBreadcrumb item =
    withJustM (preuse $ workspace . traverse . currentLocation) $ \location ->
        navigateToGraph $ location & breadcrumb . items %~ (++ [item])

enterBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command State ()
enterBreadcrumbs newBc =
    withJustM (preuse $ workspace . traverse . currentLocation) $ \location ->
        navigateToGraph $ location & breadcrumb .~ newBc

exitBreadcrumb :: Command State ()
exitBreadcrumb =
    withJustM (preuse $ workspace . traverse . currentLocation) $ \location ->
        case location ^. breadcrumb . items of
            [] -> return ()
            bc -> navigateToGraph $ location & breadcrumb . items .~ init bc

enterNode :: ExpressionNode -> Command State ()
enterNode node = when (node ^. canEnter) $ enterBreadcrumb $ mkBcItem (node ^. nodeId) where
    mkBcItem = if node ^. isDefinition then Definition else Lambda
