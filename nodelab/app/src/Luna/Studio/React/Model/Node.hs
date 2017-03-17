{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Luna.Studio.React.Model.Node
    ( module Luna.Studio.React.Model.Node
    , NodeId
    , NodeType (..)
    ) where

import           Data.Convert                         (Convertible (convert))
import           Data.Map.Lazy                        (Map)
import qualified Data.Map.Lazy                        as Map
import           Data.Position                        (Position, fromTuple, toTuple)
import           Data.Time.Clock                      (UTCTime)
import           Empire.API.Data.MonadPath            (MonadPath)
import           Empire.API.Data.Node                 (NodeId, NodeType (..))
import qualified Empire.API.Data.Node                 as Empire
import qualified Empire.API.Data.NodeMeta             as NodeMeta
import           Empire.API.Graph.CollaborationUpdate (ClientId)
import           Empire.API.Graph.NodeResultUpdate    (NodeValue)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Port         (Port, PortId, isInPort)
import qualified Luna.Studio.React.Model.Port         as Port
import           Luna.Studio.State.Collaboration      (ColorId)


data Node = Node { _nodeId                :: NodeId
                 , _name                  :: Text
                 , _nodeType              :: NodeType
                 , _canEnter              :: Bool
                 , _ports                 :: Map PortId Port
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
                  | Function [Subgraph]
                  deriving (Eq, Generic, NFData, Show)


data Subgraph = Subgraph
    { _nodes  :: [NodeId]
    , _edges  :: [Node]
    , _monads :: [MonadPath]
    } deriving (Default, Eq, Generic, NFData, Show)

data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, ColorId)
                                   , _modify :: Map ClientId  UTCTime
                                   } deriving (Default, Eq, Generic, NFData, Show)

makeLenses ''Node
makeLenses ''Subgraph
makeLenses ''Collaboration

instance Default Mode where def = Collapsed

isEdge :: Node -> Bool
isEdge node = isInputEdge node || isOutputEdge node

isInputEdge :: Node -> Bool
isInputEdge node = node ^. nodeType == InputEdge

isOutputEdge :: Node -> Bool
isOutputEdge node = node ^. nodeType == OutputEdge

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

instance Convertible Empire.Node Node where
    convert n = Node
        {- nodeId                -} (n ^. Empire.nodeId)
        {- name                  -} (n ^. Empire.name)
        {- nodeType              -} (n ^. Empire.nodeType)
        {- canEnter              -} (n ^. Empire.canEnter)
        {- ports                 -} (Map.map convert $ n ^. Empire.ports)
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


instance Convertible Node Empire.Node where
    convert n = Empire.Node
        {- nodeId                -} (n ^. nodeId)
        {- name                  -} (n ^. name)
        {- nodeType              -} (n ^. nodeType)
        {- canEnter              -} (n ^. canEnter)
        {- ports                 -} (Map.map convert $ n ^. ports)
        {- nodeMeta              -} (NodeMeta.NodeMeta (toTuple $ n ^. position) (n ^. visualizationsEnabled))
        {- code                  -} (n ^. code)
