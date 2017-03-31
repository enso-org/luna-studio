{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node
    ( module Luna.Studio.React.Model.Node
    , module X
    , ExpressionNode
    , ExpressionNodesMap
    , SidebarNode
    , SidebarNodesMap
    , toExpressionNodesMap
    , toSidebarNodesMap
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
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, ExpressionNodesMap, toExpressionNodesMap)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as ExpressionNode
import           Luna.Studio.React.Model.Node.NodeMeta       ()
import           Luna.Studio.React.Model.Node.SidebarNode    (SidebarNode, SidebarNodesMap, toSidebarNodesMap)
import qualified Luna.Studio.React.Model.Node.SidebarNode    as SidebarNode


data Node = Expression ExpressionNode
          | Sidebar    SidebarNode
          deriving (Eq, Generic, NFData, Show)

makeLenses ''Node
makePrisms ''Node

type NodesMap = HashMap NodeId Node

toNodesList :: [ExpressionNode] -> [SidebarNode] -> [Node]
toNodesList exprNodes sidebarNodes = (map Expression exprNodes) ++ (map Sidebar sidebarNodes)

toNodesMap :: [ExpressionNode] -> [SidebarNode] -> NodesMap
toNodesMap = HashMap.fromList .: map (view nodeId &&& id) .: toNodesList

instance Convertible (NodePath, Empire.Node) Node where
    convert (path, n) = case n ^. Empire.nodeType of
        Empire.ExpressionNode expr -> Expression $ convert (path, n, expr)
        Empire.InputEdge      _    -> Sidebar    $ convert (path, n, SidebarNode.Input)
        Empire.OutputEdge          -> Sidebar    $ convert (path, n, SidebarNode.Output)

instance Convertible (GraphLocation, Empire.Node) Node where
    convert (loc, n) = convert (NodePath $ loc ^. GraphLocation.breadcrumb, n)

instance IsNode Node where
    nodeLoc = lens getNodeId' setNodeId' where
        getNodeId' (Expression node)          = node ^. nodeLoc
        getNodeId' (Sidebar    node)          = node ^. nodeLoc
        setNodeId' (Expression node) nodeLoc' = Expression $ node & ExpressionNode.nodeLoc .~ nodeLoc'
        setNodeId' (Sidebar    node) nodeLoc' = Sidebar    $ node & SidebarNode.nodeLoc    .~ nodeLoc'
    ports = lens getPorts' setPorts' where
        getPorts' (Expression node)        = node ^. ports
        getPorts' (Sidebar    node)        = node ^. ports
        setPorts' (Expression node) ports' = Expression $ node & ExpressionNode.ports .~ ports'
        setPorts' (Sidebar    node) ports' = Sidebar    $ node & SidebarNode.ports    .~ ports'
    getPorts (Expression node)             = getPorts node
    getPorts (Sidebar node)                = getPorts node
    hasPort pid (Expression node)          = hasPort pid node
    hasPort pid (Sidebar node)             = hasPort pid node
    countInPorts (Expression node)         = countInPorts node
    countInPorts (Sidebar node)            = countInPorts node
    countOutPorts (Expression node)        = countOutPorts node
    countOutPorts (Sidebar node)           = countOutPorts node
    countArgPorts (Expression node)        = countArgPorts node
    countArgPorts (Sidebar node)           = countArgPorts node
    countProjectionPorts (Expression node) = countProjectionPorts node
    countProjectionPorts (Sidebar node)    = countProjectionPorts node
