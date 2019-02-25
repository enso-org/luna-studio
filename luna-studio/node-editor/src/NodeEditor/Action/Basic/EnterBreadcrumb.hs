module NodeEditor.Action.Basic.EnterBreadcrumb where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           LunaStudio.Data.Breadcrumb                 (Breadcrumb, BreadcrumbItem (Definition, Lambda), items)
import           LunaStudio.Data.GraphLocation              (breadcrumb)
import           NodeEditor.Action.Basic.ProjectManager     (navigateToGraph)
import           NodeEditor.Action.State.App                (getWorkspace)
import           NodeEditor.Batch.Workspace                 (currentLocation)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, toEnter, isDefinition, nodeId)
import           NodeEditor.State.Global                    (State)


enterBreadcrumb :: BreadcrumbItem -> Command State ()
enterBreadcrumb item =
    withJustM getWorkspace $ \workspace' -> do
        let location = workspace' ^. currentLocation
        navigateToGraph $ location & breadcrumb . items %~ (<> [item])

enterBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command State ()
enterBreadcrumbs newBc =
    withJustM getWorkspace $ \workspace' -> do
        let location = workspace' ^. currentLocation
        navigateToGraph $ location & breadcrumb .~ newBc

exitBreadcrumb :: Command State ()
exitBreadcrumb =
    withJustM getWorkspace $ \workspace' -> do
        let location = workspace' ^. currentLocation
        case location ^. breadcrumb . items of
            [] -> return ()
            bc -> navigateToGraph $ location & breadcrumb . items .~ init bc

enterNode :: ExpressionNode -> Command State ()
enterNode node = case node ^. toEnter of
    Just breadcrumb -> enterBreadcrumb breadcrumb
    _               -> return ()
