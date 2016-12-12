{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Node
    ( expandSelectedNodes
    ) where

import qualified Data.Text.Lazy                    as Text

import           Empire.API.Data.Node              (NodeId)
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Object.UITypes                    (WidgetId)
import           React.Store                       (WRef (..), widget)
import qualified React.Store                       as Store
import qualified React.Store.Node                  as Model
import qualified Reactive.Commands.Batch           as BatchCmd
import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.Commands.Node.NodeMeta   (modifyNodeMeta)
import qualified Reactive.Commands.Searcher        as Searcher
import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Utils.PreludePlus
import           Utils.Vector



expandSelectedNodes :: Command State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allSelected = all (view $ widget . Model.isExpanded) sn
        update      = if allSelected then Model.isExpanded %~ not
                                     else Model.isExpanded .~ True
    forM_ sn $
        Store.modify_ update . _ref

visualizationsToggled :: WidgetId -> NodeId -> Bool -> Command State ()
visualizationsToggled _ nid val = modifyNodeMeta nid (NodeMeta.displayResult .~ val)

codeChanged :: NodeId -> Text -> Command State ()
codeChanged nodeId newCode = do
    BatchCmd.setCode nodeId newCode

editNodeExpression :: NodeId -> Command State ()
editNodeExpression nodeId = do
    exprMay     <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . Node.nodeType . Node.expression
    nodeRefMay <- Global.getNode nodeId
    withJust exprMay $ \expr -> withJust nodeRefMay $ \nodeRef -> do
        node <- Store.get nodeRef
        pos <- zoom Global.camera $ Camera.workspaceToScreen $ node ^. Model.position
        let halfCharWidth = 4
            offset = Vector2 (-10 - halfCharWidth * fromIntegral (Text.length expr)) (-59)
        Searcher.openEdit expr nodeId $ pos + offset
