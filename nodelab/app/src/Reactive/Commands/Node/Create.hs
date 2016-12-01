{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Create
    ( addNode
    , addDummyNode
    , registerNode
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Control.Monad.State               (modify)
import qualified Data.Text.Lazy                    as Text

import           Object.UITypes                    (WidgetId)
import           React.Store                       (Ref, WRef (..), ref, widget)
import qualified React.Store                       as Store
import qualified React.Store.Node                  as Model
import qualified React.Store.NodeEditor            as NodeEditor

import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Graph           (focusNode)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.Commands.Node.NodeMeta   (modifyNodeMeta)
import           Reactive.Commands.Node.Remove     (removeSelectedNodes)
import qualified Reactive.Commands.UIRegistry      as UICmd
import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Global             (State, inRegistry)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Reactive.State.UIRegistry         (addHandler, sceneGraphId)

import qualified Reactive.Commands.Batch           as BatchCmd

import           Data.HMap.Lazy                    (HTMap)
import qualified UI.Handlers.Node                  as UINode

import qualified Empire.API.Data.Breadcrumb        as Breadcrumb
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.NodeMeta          as NodeMeta

import qualified Reactive.Commands.NodeSearcher    as NS


addNode :: Node -> Command State ()
addNode node = do
    zoom Global.graph $ modify (Graph.addNode node)
    nodeRef <- registerNode node
    focusNode nodeRef

addDummyNode :: Node -> Command State ()
addDummyNode dummyNode = do
    mayNode <- preuse $ Global.graph . Graph.nodesMap . ix (dummyNode ^. Node.nodeId)
    maybe (addNode dummyNode) (const $ return ()) mayNode

registerNode :: Node -> Command State (Ref Model.Node)
registerNode node = do
    let nodeModel = Model.fromNode node
        nodeId    = node ^. Node.nodeId
    Global.withNodeEditor $ Store.modifyM $ do
        ref <- lift $ Store.create nodeModel
        NodeEditor.nodes . at nodeId ?= ref
        return ref

--TODO[react]
-- nodeHandlers :: Node -> HTMap
-- nodeHandlers node = addHandler (UINode.RemoveNodeHandler            removeSelectedNodes)
--                   $ addHandler (UINode.RenameNodeHandler            $ \_ nodeId name -> BatchCmd.renameNode nodeId name)
--                   $ addHandler (UINode.ChangeInputNodeTypeHandler   $ \_ nodeId name -> BatchCmd.setInputNodeType nodeId name)
--                   $ addHandler (UINode.FocusNodeHandler             focusNode)
--                   $ addHandler (UINode.ExpandNodeHandler            expandSelectedNodes)
--                   $ addHandler (UINode.EditNodeExpressionHandler    editNodeExpression)
--                   $ addHandler (UINode.VisualizationsToggledHandler visualizationsToggled)
--                   $ addHandler (UINode.CodeChangedHandler           codeChanged)

visualizationsToggled :: WidgetId -> NodeId -> Bool -> Command Global.State ()
visualizationsToggled _ nid val = modifyNodeMeta nid (NodeMeta.displayResult .~ val)

codeChanged :: NodeId -> Text -> Command Global.State ()
codeChanged nodeId newCode = do
    BatchCmd.setCode nodeId newCode

expandSelectedNodes :: Command Global.State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allSelected = all (view $ widget . Model.isExpanded) sn
        update      = if allSelected then Model.isExpanded %~ not
                                     else Model.isExpanded .~ True
    forM_ sn $
        Store.modify_ update . _ref

editNodeExpression :: NodeId -> Command Global.State ()
editNodeExpression nodeId = do
    exprMay     <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . Node.nodeType . Node.expression
    nodeRefMay <- Global.getNode nodeId
    withJust exprMay $ \expr -> withJust nodeRefMay $ \nodeRef -> do
        node <- Store.get nodeRef
        pos <- zoom Global.camera $ Camera.workspaceToScreen $ node ^. Model.position
        let halfCharWidth = 4
            offset = Vector2 (-10 - halfCharWidth * fromIntegral (Text.length expr)) (-59)
        NS.openEdit expr nodeId $ pos + offset
