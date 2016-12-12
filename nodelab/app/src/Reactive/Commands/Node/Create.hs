{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Create
    ( addNode
    , addDummyNode
    , registerNode
    ) where

import           Control.Monad.State       (modify)

import           Empire.API.Data.Node      (Node)
import qualified Empire.API.Data.Node      as Node
import           React.Store               (Ref)
import qualified React.Store               as Store
import qualified React.Store.Node          as Model
import qualified React.Store.NodeEditor    as NodeEditor
import           Reactive.Commands.Command (Command)
import           Reactive.Commands.Graph   (focusNode)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph
import           Utils.PreludePlus


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
-- nodeHandlers node = addHandler (UINode.RenameNodeHandler            $ \_ nodeId name -> BatchCmd.renameNode nodeId name)
--                   $ addHandler (UINode.ChangeInputNodeTypeHandler   $ \_ nodeId name -> BatchCmd.setInputNodeType nodeId name)
--                   $ addHandler (UINode.FocusNodeHandler             focusNode)
--                   $ addHandler (UINode.ExpandNodeHandler            expandSelectedNodes)
--                   $ addHandler (UINode.VisualizationsToggledHandler visualizationsToggled)
--                   $ addHandler (UINode.CodeChangedHandler           codeChanged)
