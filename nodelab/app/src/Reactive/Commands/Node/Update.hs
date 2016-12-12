{-# LANGUAGE OverloadedStrings #-}

module Reactive.Commands.Node.Update
    ( updateNode
    , updateNodeValue
    , updateNodeProfilingData
    , updateExpression
    ) where

import           Utils.PreludePlus

import           Control.Arrow
import           Control.Monad.State               (modify)

import qualified Data.Map.Lazy                     as Map

import qualified React.Store                       as Store
import qualified React.Store.Node                  as Model

import qualified Reactive.Commands.Batch           as BatchCmd
import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Node.Create     (addNode)

import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph

import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node

import           Empire.API.Graph.NodeResultUpdate (NodeValue)

import           Object.Widget.Node                (makePorts)
import           Object.Widget.Port                (portRef)


updateNode :: Node -> Command State ()
updateNode node = do
    let nodeId  = node ^. Node.nodeId
    inGraph <- preuse $ Global.graph . Graph.nodesMap . ix nodeId
    case inGraph of
        Just _existingNode -> updateExistingNode node
        Nothing            -> addNode            node

updateExistingNode :: Node -> Command State ()
updateExistingNode node = do
    let nodeId  = node ^. Node.nodeId
    zoom Global.graph $ modify (Graph.addNode node)
    Global.withNode nodeId $ mapM_ $ Store.modifyM_ $ do
        case node ^. Node.nodeType of
            Node.ExpressionNode expression -> Model.expression .= expression
            _                              -> return ()
        Model.code  .= (node ^. Node.code)
        Model.ports .= (Map.fromList $ map (view portRef &&& id) $ makePorts node)
        -- TODO: obsluzyc to ze moga zniknac polaczenia
    -- TODO[react]: find out if we need this
    -- updateConnectionsForNodes [nodeId]

updateNodeValue :: NodeId -> NodeValue -> Command State ()
updateNodeValue nid val =
    Global.withNode nid $ mapM_ $ Store.modify_ $
        Model.value ?~ val

updateNodeProfilingData :: NodeId -> Integer -> Command State ()
updateNodeProfilingData nodeId execTime =
    Global.withNode nodeId $ mapM_ $ Store.modify_ $
        Model.execTime ?~ execTime

updateExpression :: NodeId -> Text -> Command State ()
updateExpression nodeId expr = do
    BatchCmd.updateNodeExpression nodeId expr
