{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node
    ( module Luna.Studio.React.Model.Node
    , NodeId
    , EdgeNode
    , EdgeNodesMap
    , toEdgeNodesMap
    , ExpressionNode
    , ExpressionNodesMap
    , toExpressionNodesMap
    ) where

import           Control.Arrow                               ((&&&))
import           Data.Convert                                (Convertible (convert))
import           Data.HashMap.Strict                         (HashMap)
import qualified Data.HashMap.Strict                         as HashMap
import           Empire.API.Data.Node                        (NodeId)
import qualified Empire.API.Data.Node                        as Empire
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.EdgeNode       (EdgeNode, EdgeNodesMap, toEdgeNodesMap)
import qualified Luna.Studio.React.Model.Node.EdgeNode       as EdgeNode
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, ExpressionNodesMap, toExpressionNodesMap)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as ExpressionNode
import           Luna.Studio.React.Model.Node.NodeMeta       ()
import           Luna.Studio.React.Model.Port                (Port, PortId, PortsMap)


data Node = Expression ExpressionNode
          | Edge       EdgeNode
          deriving (Eq, Generic, NFData, Show)

makeLenses ''Node
makePrisms ''Node

type NodesMap = HashMap NodeId Node

toNodesList :: [ExpressionNode] -> [EdgeNode] -> [Node]
toNodesList exprNodes edgeNodes = (map Expression exprNodes) ++ (map Edge edgeNodes)

toNodesMap :: [ExpressionNode] -> [EdgeNode] -> NodesMap
toNodesMap = HashMap.fromList .: map (view nodeId &&& id) .: toNodesList


instance Convertible Empire.Node Node where
    convert n = case n ^. Empire.nodeType of
        Empire.ExpressionNode expr -> Expression $ convert (n, expr)
        Empire.InputEdge           -> Edge       $ convert (n, EdgeNode.InputEdge)
        Empire.OutputEdge          -> Edge       $ convert (n, EdgeNode.OutputEdge)

class IsNode a where
    nodeId               :: Lens' a NodeId
    ports                :: Lens' a PortsMap
    getPorts             :: a -> [Port]
    hasPort              :: PortId -> a -> Bool
    countInPorts         :: a -> Int
    countOutPorts        :: a -> Int
    countArgPorts        :: a -> Int
    countProjectionPorts :: a -> Int

instance IsNode Node where
    nodeId = lens getNodeId' setNodeId' where
        getNodeId' (Expression node)         = node ^. nodeId
        getNodeId' (Edge       node)         = node ^. nodeId
        setNodeId' (Expression node) nodeId' = Expression $ node & ExpressionNode.nodeId .~ nodeId'
        setNodeId' (Edge       node) nodeId' = Edge       $ node & EdgeNode.nodeId       .~ nodeId'
    ports = lens getPorts' setPorts' where
        getPorts' (Expression node)        = node ^. ports
        getPorts' (Edge       node)        = node ^. ports
        setPorts' (Expression node) ports' = Expression $ node & ExpressionNode.ports .~ ports'
        setPorts' (Edge       node) ports' = Edge       $ node & EdgeNode.ports       .~ ports'
    getPorts (Expression node)             = getPorts node
    getPorts (Edge node)                   = getPorts node
    hasPort pid (Expression node)          = hasPort pid node
    hasPort pid (Edge node)                = hasPort pid node
    countInPorts (Expression node)         = countInPorts node
    countInPorts (Edge node)               = countInPorts node
    countOutPorts (Expression node)        = countOutPorts node
    countOutPorts (Edge node)              = countOutPorts node
    countArgPorts (Expression node)        = countArgPorts node
    countArgPorts (Edge node)              = countArgPorts node
    countProjectionPorts (Expression node) = countProjectionPorts node
    countProjectionPorts (Edge node)       = countProjectionPorts node


instance IsNode EdgeNode where
    nodeId               = EdgeNode.nodeId
    ports                = EdgeNode.ports
    getPorts             = EdgeNode.getPorts
    hasPort              = EdgeNode.hasPort
    countInPorts         = EdgeNode.countInPorts
    countOutPorts        = EdgeNode.countOutPorts
    countArgPorts        = EdgeNode.countArgPorts
    countProjectionPorts = EdgeNode.countProjectionPorts

instance IsNode ExpressionNode where
    nodeId               = ExpressionNode.nodeId
    ports                = ExpressionNode.ports
    getPorts             = ExpressionNode.getPorts
    hasPort              = ExpressionNode.hasPort
    countInPorts         = ExpressionNode.countInPorts
    countOutPorts        = ExpressionNode.countOutPorts
    countArgPorts        = ExpressionNode.countArgPorts
    countProjectionPorts = ExpressionNode.countProjectionPorts
