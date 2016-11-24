{-# LANGUAGE OverloadedStrings #-}

module Reactive.Commands.Node.Update
    ( updateNode
    , updateNodeValue
    , updateNodeProfilingData
    , updateExpression
    ) where

import           Utils.PreludePlus

import           Control.Monad.State                  (modify)
import qualified Data.Text.Lazy                       as Text

import qualified React.Store                          as Store
import qualified React.Store.Node                     as Model

import           Reactive.Commands.Command            (Command)
import           Reactive.Commands.Graph              (nodeIdToWidgetId, updateConnectionsForNodes)
import qualified Reactive.Commands.UIRegistry         as UICmd
import           Reactive.State.Global                (State, inRegistry)
import qualified Reactive.State.Global                as Global
import qualified Reactive.State.Graph                 as Graph

import           Empire.API.Data.Node                 (Node, NodeId)
import qualified Empire.API.Data.Node                 as Node
import qualified Empire.API.Graph.NodeResultUpdate    as NodeResult

import qualified Reactive.Commands.Batch              as BatchCmd
import           Reactive.Commands.Node.Create        (addNode)
import           Reactive.Commands.Node.Ports         (displayPorts)
import           Reactive.Commands.Node.Visualization (limitString, showError, visualizeError, visualizeNodeValueReprs)

errorLen :: Int
errorLen = 40

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
    maybeWidgetId <- nodeIdToWidgetId nodeId
    zoom Global.graph $ modify (Graph.addNode node)
    Global.inNode nodeId $ mapM_ $ Store.modifyM_ $ \model -> do
        -- displayPorts widgetId node --TODO react
        let model' = model & case node ^. Node.nodeType of
                Node.ExpressionNode expression -> Model.expression .~ expression
                _                              -> id
        return $ model' & Model.code .~ (node ^. Node.code)
        -- TODO: obsluzyc to ze moga zniknac polaczenia
    updateConnectionsForNodes [nodeId]

updateNodeValue :: NodeId -> NodeResult.NodeValue -> Command State ()
updateNodeValue nid val =
    Global.inNode nid $ mapM_ $ Store.modify_ $
        -- removeVisualization widgetId
        case val of
            NodeResult.Value name [] ->
                (Model.value   .~ name)
                . (Model.isError .~ False)
            NodeResult.Value name valueReprs ->
                (Model.value   .~ name)
                . (Model.isError .~ False)
                -- visualizeNodeValueReprs widgetId valueReprs --TODO react
            NodeResult.Error msg ->
                (Model.value   .~ limitString errorLen (Text.pack $ showError msg))
                . (Model.isError .~ True)
                -- visualizeError widgetId msg --TODO react

updateNodeProfilingData :: NodeId -> Integer -> Command State ()
updateNodeProfilingData nodeId execTime =
    Global.inNode nodeId $ mapM_ $ Store.modify_ $
        Model.execTime ?~ execTime

updateExpression :: NodeId -> Text -> Command State ()
updateExpression nodeId expr = do
    BatchCmd.updateNodeExpression nodeId expr
