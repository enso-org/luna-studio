{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Action.Node.Update
    ( updateNode
    , typecheckNode
    , updateNodeValue
    , updateNodeProfilingData
    , updateExpression
    ) where

import           Control.Arrow                      ((&&&))
import           Control.Monad.State                (modify)
import qualified Data.Map.Lazy                      as Map
import           Empire.API.Data.Node               (Node, NodeId, NodeTypecheckerUpdate)
import qualified Empire.API.Data.Node               as Node
import           Empire.API.Graph.NodeResultUpdate  (NodeValue)
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Update    (updateConnectionsForNodes)
import           Luna.Studio.Action.Node.Create     (addNode)
import           Luna.Studio.Action.Port.Self       (removeSelfIfNeeded)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (makePorts)
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Model.Port       (portRef)
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph


updateNode :: Node -> Command State ()
updateNode node = do
    let nodeId  = node ^. Node.nodeId
    inGraph <- preuse $ Global.graph . Graph.nodesMap . ix nodeId
    case inGraph of
        Just _existingNode -> updateExistingNode node
        Nothing            -> addNode            node

typecheckNode :: NodeTypecheckerUpdate -> Command State ()
typecheckNode node = do
    let nodeId = node ^. Node.tcNodeId
    inGraph <- preuse $ Global.graph . Graph.nodesMap . ix nodeId
    case inGraph of
        Just existingNode  -> do
            let updatedNode = existingNode & Node.ports .~ (node ^. Node.tcPorts)
            updateExistingNode updatedNode
        -- typecheck non-existing node?
        Nothing            -> return ()

updateExistingNode :: Node -> Command State ()
updateExistingNode node = do
    let nodeId  = node ^. Node.nodeId
    updateConnectionsForNodes [nodeId]
    zoom Global.graph $ modify (Graph.addNode node)
    mayModel <- Global.getNode nodeId
    withJust mayModel $ \model -> do
        let ports = Map.fromList (map (view portRef &&& id) $ makePorts node)
            code  = node ^. Node.code
            expr  = case node ^. Node.nodeType of
                Node.ExpressionNode expression -> expression
                _                              -> model ^. Model.expression
        newModel <- removeSelfIfNeeded $ model & Model.ports      .~ ports
                                               & Model.code       .~ code
                                               & Model.expression .~ expr
        Global.modifyNodeEditor $ NodeEditor.nodes . at nodeId ?= newModel

updateNodeValue :: NodeId -> NodeValue -> Command State ()
updateNodeValue nodeId val =
    Global.modifyNode nodeId $ Model.value ?= val

updateNodeProfilingData :: NodeId -> Integer -> Command State ()
updateNodeProfilingData nodeId execTime =
    Global.modifyNode nodeId $ Model.execTime ?= execTime

updateExpression :: NodeId -> Text -> Command State ()
updateExpression = BatchCmd.updateNodeExpression
