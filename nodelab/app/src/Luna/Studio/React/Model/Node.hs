{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node
    ( module Luna.Studio.React.Model.Node
    , NodeId
    ) where

import           Control.Arrow                        ((&&&))
import           Data.Convert                         (Convertible (convert))
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.Map.Lazy                        (Map)
import qualified Data.Map.Lazy                        as Map
import           Data.Position                        (Position, fromTuple, toTuple)
import           Data.Time.Clock                      (UTCTime)
import           Empire.API.Data.Breadcrumb           (BreadcrumbItem)
import           Empire.API.Data.MonadPath            (MonadPath)
import           Empire.API.Data.Node                 (NodeId)
import qualified Empire.API.Data.Node                 as Empire
import           Empire.API.Data.NodeMeta             (NodeMeta (NodeMeta))
import qualified Empire.API.Data.NodeMeta             as NodeMeta
import           Empire.API.Graph.CollaborationUpdate (ClientId)
import           Empire.API.Graph.NodeResultUpdate    (NodeValue)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.EdgeNode     (EdgeNode, EdgeNodesMap)
import qualified Luna.Studio.React.Model.EdgeNode     as EdgeNode
import           Luna.Studio.React.Model.Port         (Port, PortId, PortsMap, isInPort)
import qualified Luna.Studio.React.Model.Port         as Port
import           Luna.Studio.State.Collaboration      (ColorId)


data Node = Node { _nodeId                :: NodeId
                 , _name                  :: Text
                 , _expression            :: Text
                 , _canEnter              :: Bool
                 , _ports                 :: PortsMap
                 , _position              :: Position
                 , _visualizationsEnabled :: Bool
                 , _code                  :: Maybe Text

                 , _value                 :: Maybe NodeValue
                 , _zPos                  :: Int
                 , _isSelected            :: Bool
                 , _mode                  :: Mode
                 , _nameEdit              :: Maybe Text
                 , _execTime              :: Maybe Integer
                 , _collaboration         :: Collaboration
                 } deriving (Eq, Generic, NFData, Show)

data Mode = Collapsed
          | Expanded ExpandedMode
          deriving (Eq, Generic, NFData, Show)

data ExpandedMode = Editor
                  | Controls
                  | Function (Map BreadcrumbItem Subgraph)
                  deriving (Eq, Generic, NFData, Show)

data Subgraph = Subgraph
    { _nodes       :: NodesMap
    , _edgeNodes   :: EdgeNodesMap
    , _monads      :: [MonadPath]
    } deriving (Default, Eq, Generic, NFData, Show)

data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, ColorId)
                                   , _modify :: Map ClientId  UTCTime
                                   } deriving (Default, Eq, Generic, NFData, Show)

type NodesMap = HashMap NodeId Node

makeLenses ''Collaboration
makeLenses ''Node
makeLenses ''Subgraph
makePrisms ''ExpandedMode
makePrisms ''Mode

instance Convertible (Empire.Node, Text) Node where
    convert (n, expr) = Node
        {- nodeId                -} (n ^. Empire.nodeId)
        {- name                  -} (n ^. Empire.name)
        {- expression            -} expr
        {- canEnter              -} (n ^. Empire.canEnter)
        {- ports                 -} (convert <$> n ^. Empire.ports)
        {- position              -} (fromTuple $ n ^. Empire.position)
        {- visualizationsEnabled -} (n ^. Empire.nodeMeta . NodeMeta.displayResult)
        {- code                  -} (n ^. Empire.code)

        {- value                 -} def
        {- zPos                  -} def
        {- isSelected            -} False
        {- mode                  -} def
        {- nameEdit              -} def
        {- execTime              -} def
        {- collaboration         -} def

instance Convertible Empire.Node (Either Node EdgeNode) where
    convert n = case n ^. Empire.nodeType of
        Empire.ExpressionNode expr -> Left  $ convert (n, expr)
        Empire.InputEdge           -> Right $ convert (n, EdgeNode.InputEdge)
        Empire.OutputEdge          -> Right $ convert (n, EdgeNode.OutputEdge)

instance Convertible Node Empire.Node where
    convert n = Empire.Node
        {- nodeId   -} (n ^. nodeId)
        {- name     -} (n ^. name)
        {- nodeType -} (Empire.ExpressionNode $ n ^. expression)
        {- canEnter -} (n ^. canEnter)
        {- ports    -} (convert <$> n ^. ports)
        {- nodeMeta -} (NodeMeta.NodeMeta (toTuple $ n ^. position) (n ^. visualizationsEnabled))
        {- code     -} (n ^. code)


instance Convertible (Position, Bool) NodeMeta where
    convert (pos, dispRes) = NodeMeta (toTuple pos) dispRes

instance Convertible NodeMeta (Position, Bool) where
    convert (NodeMeta pos dispRes) = (fromTuple pos, dispRes)


instance Convertible (NodeId, Position, Bool) (NodeId, NodeMeta) where
    convert (nid, pos, dispRes) = (nid, convert (pos, dispRes))

instance Convertible (NodeId, NodeMeta) (NodeId, Position, Bool) where
    convert (nid, NodeMeta pos dispRes) = (nid, fromTuple pos, dispRes)

instance Default Mode where def = Collapsed

toNodesMap :: [Node] -> NodesMap
toNodesMap = HashMap.fromList . map (view nodeId &&& id)

subgraphs :: Getter Node [Subgraph]
subgraphs = to (toListOf $ mode . _Expanded . _Function . traverse)

isMode :: Mode -> Node -> Bool
isMode mode' node = node ^. mode == mode'

isExpanded :: Node -> Bool
isExpanded node = case node ^. mode of
    Expanded _ -> True
    _          -> False

isExpandedControls :: Node -> Bool
isExpandedControls = isMode (Expanded Controls)

isExpandedFunction :: Node -> Bool
isExpandedFunction node = case node ^. mode of
    Expanded (Function _) -> True
    _                     -> False

isCollapsed :: Node -> Bool
isCollapsed = isMode Collapsed

isLiteral :: Node -> Bool
isLiteral node = not $ any isInPort portIds where
    portIds = Map.keys $ node ^. ports

getPorts :: Node -> [Port]
getPorts = Map.elems . view ports

hasPort :: PortId -> Node -> Bool
hasPort pid = Map.member pid . view ports

countInPorts :: Node -> Int
countInPorts = Port.countInPorts . Map.keys . (view ports)

countOutPorts :: Node -> Int
countOutPorts = Port.countOutPorts . Map.keys . (view ports)

countArgPorts :: Node -> Int
countArgPorts = Port.countArgPorts . Map.keys . (view ports)

countProjectionPorts :: Node -> Int
countProjectionPorts = Port.countProjectionPorts . Map.keys . (view ports)
