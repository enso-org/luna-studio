{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Action.Node.Update
    ( updateNode
    , updateNodeValue
    , updateNodeProfilingData
    , updateExpression
    ) where

import           Control.Arrow                     ((&&&))
import           Control.Monad.State               (modify)
import qualified Data.Map.Lazy                     as Map
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Graph.NodeResultUpdate (NodeValue)
import qualified Luna.Studio.Action.Batch          as BatchCmd
import           Luna.Studio.Action.Command        (Command)
import           Luna.Studio.Action.Graph.Update   (updateConnectionsForNodes)
import           Luna.Studio.Action.Node.Create    (addNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node      (makePorts)
import qualified Luna.Studio.React.Model.Node      as Model
import           Luna.Studio.React.Model.Port      (portRef)
import qualified Luna.Studio.React.Store           as Store
import           Luna.Studio.State.Global          (State)
import qualified Luna.Studio.State.Global          as Global
import qualified Luna.Studio.State.Graph           as Graph


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
        -- Comment[LJK]: Is this real issue???
    updateConnectionsForNodes [nodeId]

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
