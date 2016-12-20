{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Commands.Node.Create
    ( addNode
    , addDummyNode
    , registerNode
    ) where

import           Control.Monad.State                (modify)

import           Empire.API.Data.Node               (Node)
import qualified Empire.API.Data.Node               as Node
import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Commands.Graph         (focusNode)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Store            (Ref)
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph


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
-- nodeHandlers node = addHandler (UINode.ChangeInputNodeTypeHandler   $ \_ nodeId name -> BatchCmd.setInputNodeType nodeId name)
--                   $ addHandler (UINode.FocusNodeHandler             focusNode)
--                   $ addHandler (UINode.ExpandNodeHandler            expandSelectedNodes)
--                   $ addHandler (UINode.VisualizationsToggledHandler visualizationsToggled)
--                   $ addHandler (UINode.CodeChangedHandler           codeChanged)
