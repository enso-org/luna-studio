{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Commands.Node
    ( expandSelectedNodes
    , editExpression
    , enter
    , exit
    , rename
    , tryEnter
    , visualizationsToggled
    ) where

import           Empire.API.Data.Breadcrumb           (BreadcrumbItem (..))
import qualified Empire.API.Data.Breadcrumb           as Breadcrumb
import qualified Empire.API.Data.GraphLocation        as GraphLocation
import           Empire.API.Data.Node                 (Node, NodeId)
import qualified Empire.API.Data.Node                 as Node
import qualified Empire.API.Data.NodeMeta             as NodeMeta
import qualified Luna.Studio.Batch.Workspace          as Workspace
import           Luna.Studio.Commands.Command         (Command)
import           Luna.Studio.Commands.Graph.Selection (selectedNodes)
import           Luna.Studio.Commands.Node.NodeMeta   (modifyNodeMeta)
import           Luna.Studio.Commands.ProjectManager  as ProjectManager
import qualified Luna.Studio.Commands.Searcher        as Searcher
import           Luna.Studio.Prelude
import           Luna.Studio.React.Store              (WRef (..), widget)
import qualified Luna.Studio.React.Store              as Store
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph
import qualified Object.Widget.Node                   as Model



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

rename :: NodeId -> Text -> Command State ()
rename nodeId name = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.name .= name
    Global.withNode nodeId $
        mapM_ $ Store.modify_ (Model.name .~ name)

expandSelectedNodes :: Command State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allSelected = all (view $ widget . Model.isExpanded) sn
        update      = if allSelected then Model.isExpanded %~ not
                                     else Model.isExpanded .~ True
    forM_ sn $
        Store.modify_ update . _ref

visualizationsToggled :: NodeId -> Bool -> Command State ()
visualizationsToggled nid val = do
    modifyNodeMeta nid $ NodeMeta.displayResult .~ val
    Global.withNode nid $ mapM_ $ Store.modify_ (Model.visualizationsEnabled .~ val)

editExpression :: NodeId -> Command State ()
editExpression nodeId = do
    exprMay     <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . Node.nodeType . Node.expression
    nodeRefMay <- Global.getNode nodeId
    withJust exprMay $ \expr -> withJust nodeRefMay $ \nodeRef -> do
        node <- Store.get nodeRef
        let pos = round <$> node ^. Model.position
        Searcher.openEdit expr nodeId $ pos
