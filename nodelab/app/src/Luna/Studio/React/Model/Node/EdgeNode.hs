{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node.EdgeNode
    ( module Luna.Studio.React.Model.Node.EdgeNode
    , module X
    , NodeId
    , NodeLoc
    ) where

import           Control.Arrow                  ((&&&))
import           Data.Convert                   (Convertible (convert))
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.Map.Lazy                  as Map
import           Empire.API.Data.Node           (NodeId, inputEdgePorts)
import qualified Empire.API.Data.Node           as Empire
import           Empire.API.Data.NodeLoc        (NodeLoc (NodeLoc), NodePath)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.IsNode as X (IsNode (..))
import           Luna.Studio.React.Model.Port   (PortsMap, toPortsMap)
import qualified Luna.Studio.React.Model.Port   as Port


data EdgeType = InputEdge | OutputEdge  deriving (Generic, Eq, NFData, Show)
data EdgeMode = AddRemove
              | MoveConnect
              deriving (Generic, Eq, NFData, Show)

instance Default EdgeMode where
    def = MoveConnect

data EdgeNode = EdgeNode { _nodeLoc'              :: NodeLoc
                         , _edgeType              :: EdgeType
                         , _ports'                :: PortsMap
                         , _mode                  :: EdgeMode
                         } deriving (Eq, Generic, NFData, Show)

makeLenses ''EdgeNode

type EdgeNodesMap = HashMap NodeId EdgeNode

instance Convertible (NodePath, Empire.Node, EdgeType) EdgeNode where
    convert (path, n, type') = EdgeNode
        {- nodeLoc  -} (NodeLoc path (n ^. Empire.nodeId))
        {- edgeType -} type'
        {- ports    -} ( case type' of
            OutputEdge -> convert <$> n ^. Empire.ports
            InputEdge  ->
                toPortsMap . convert . maybe [] id $ n ^? Empire.nodeType . inputEdgePorts
            )
        {- mode     -} def

instance IsNode EdgeNode where
    nodeLoc              = nodeLoc'
    ports                = ports'
    getPorts             = Map.elems . view ports
    hasPort pid          = Map.member pid . view ports
    countInPorts         = Port.countInPorts . Map.keys . (view ports)
    countOutPorts        = Port.countOutPorts . Map.keys . (view ports)
    countArgPorts        = Port.countArgPorts . Map.keys . (view ports)
    countProjectionPorts = Port.countProjectionPorts . Map.keys . (view ports)

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
