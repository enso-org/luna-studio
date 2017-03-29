{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node
    ( module Luna.Studio.React.Model.Node
    , module X
    , EdgeNode
    , EdgeNodesMap
    , ExpressionNode
    , ExpressionNodesMap
    , toEdgeNodesMap
    , toExpressionNodesMap
    ) where

import           Control.Arrow                               ((&&&))
import           Data.Convert                                (Convertible (convert))
import           Data.HashMap.Strict                         (HashMap)
import qualified Data.HashMap.Strict                         as HashMap
import           Empire.API.Data.GraphLocation               (GraphLocation)
import qualified Empire.API.Data.GraphLocation               as GraphLocation
import           Empire.API.Data.Node                        as X (NodeId)
import qualified Empire.API.Data.Node                        as Empire
import           Empire.API.Data.NodeLoc                     as X (NodeLoc (NodeLoc), NodePath (NodePath))
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.IsNode              as X (IsNode (..))
import           Luna.Studio.React.Model.Node.EdgeNode       (EdgeNode, EdgeNodesMap, toEdgeNodesMap)
import qualified Luna.Studio.React.Model.Node.EdgeNode       as EdgeNode
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, ExpressionNodesMap, toExpressionNodesMap)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as ExpressionNode
import           Luna.Studio.React.Model.Node.NodeMeta       ()


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

instance Convertible (NodePath, Empire.Node) Node where
    convert (path, n) = case n ^. Empire.nodeType of
        Empire.ExpressionNode expr -> Expression $ convert (path, n, expr)
        Empire.InputEdge      _    -> Edge       $ convert (path, n, EdgeNode.InputEdge)
        Empire.OutputEdge          -> Edge       $ convert (path, n, EdgeNode.OutputEdge)

instance Convertible (GraphLocation, Empire.Node) Node where
    convert (loc, n) = convert (NodePath $ loc ^. GraphLocation.breadcrumb, n)

instance IsNode Node where
    nodeLoc = lens getNodeId' setNodeId' where
        getNodeId' (Expression node)         = node ^. nodeLoc
        getNodeId' (Edge       node)         = node ^. nodeLoc
        setNodeId' (Expression node) nodeLoc' = Expression $ node & ExpressionNode.nodeLoc .~ nodeLoc'
        setNodeId' (Edge       node) nodeLoc' = Edge       $ node & EdgeNode.nodeLoc       .~ nodeLoc'
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
