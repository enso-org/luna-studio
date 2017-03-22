{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node.EdgeNode
    ( module Luna.Studio.React.Model.Node.EdgeNode
    , NodeId
    ) where

import           Control.Arrow                ((&&&))
import           Data.Convert                 (Convertible (convert))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import qualified Data.Map.Lazy                as Map
import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Empire
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Port (Port, PortId, PortsMap)
import qualified Luna.Studio.React.Model.Port as Port


data EdgeType = InputEdge | OutputEdge  deriving (Generic, Eq, NFData, Show)
data EdgeMode = AddRemove | MoveConnect deriving (Generic, Eq, NFData, Show)

instance Default EdgeMode where
    def = MoveConnect

data EdgeNode = EdgeNode { _nodeId                :: NodeId
                         , _edgeType              :: EdgeType
                         , _ports                 :: PortsMap
                         , _mode                  :: EdgeMode
                         } deriving (Eq, Generic, NFData, Show)

makeLenses ''EdgeNode

type EdgeNodesMap = HashMap NodeId EdgeNode

instance Convertible (Empire.Node, EdgeType) EdgeNode where
    convert (n, type') = EdgeNode
        {- nodeId   -} (n ^. Empire.nodeId)
        {- edgeType -} type'
        {- ports    -} (convert <$> n ^. Empire.ports)
        {- mode     -} def

toEdgeNodesMap :: [EdgeNode] -> EdgeNodesMap
toEdgeNodesMap = HashMap.fromList . map (view nodeId &&& id)


isInputEdge :: EdgeNode -> Bool
isInputEdge n = n ^. edgeType == InputEdge

isOutputEdge :: EdgeNode -> Bool
isOutputEdge n = n ^. edgeType == OutputEdge

isInMode :: EdgeMode -> EdgeNode -> Bool
isInMode mode' n = n ^. mode == mode'

isInAddRemoveMode :: EdgeNode -> Bool
isInAddRemoveMode = isInMode AddRemove

isInMoveConnectMode :: EdgeNode -> Bool
isInMoveConnectMode = isInMode MoveConnect

getPorts :: EdgeNode -> [Port]
getPorts = Map.elems . view ports

hasPort :: PortId -> EdgeNode -> Bool
hasPort pid = Map.member pid . view ports

countInPorts :: EdgeNode -> Int
countInPorts = Port.countInPorts . Map.keys . (view ports)

countOutPorts :: EdgeNode -> Int
countOutPorts = Port.countOutPorts . Map.keys . (view ports)

countArgPorts :: EdgeNode -> Int
countArgPorts = Port.countArgPorts . Map.keys . (view ports)

countProjectionPorts :: EdgeNode -> Int
countProjectionPorts = Port.countProjectionPorts . Map.keys . (view ports)
