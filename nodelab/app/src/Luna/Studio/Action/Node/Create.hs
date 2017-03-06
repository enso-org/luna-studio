{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Action.Node.Create
    () where

import           Control.Monad.State                (modify)

import           Empire.API.Data.Node               (Node)
import qualified Empire.API.Data.Node               as Node
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         ()
import           Luna.Studio.Action.ConnectionPen   ()
import           Luna.Studio.Action.Graph.Focus     (focusNode)
import           Luna.Studio.Action.Port.Self       (showOrHideSelfPort)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (connectAction, penConnectAction)
import           Luna.Studio.State.Global           (State, checkAction)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph

-- addNode :: Node -> Command State ()
-- addNode node = do
--     zoom Global.graph $ modify (Graph.addNode node)
--     nodeModel <- registerNode node
--     focusNode $ nodeModel ^. Model.nodeId
--
-- addDummyNode :: Node -> Command State ()
-- addDummyNode dummyNode = do
--     mayNode <- preuse $ Global.graph . Graph.nodesMap . ix (dummyNode ^. Node.nodeId)
--     maybe (addNode dummyNode) (const $ return ()) mayNode
--
-- registerNode :: Node -> Command State Model.Node
-- registerNode node = do
--     let nodeId    = node ^. Node.nodeId
--     mayConnect    <- checkAction connectAction
--     mayPenConnect <- checkAction penConnectAction
--     nodeModel     <- showOrHideSelfPort mayConnect mayPenConnect $ Model.fromNode node
--     Global.modifyNodeEditor $ NodeEditor.nodes . at nodeId ?= nodeModel
--     return nodeModel
