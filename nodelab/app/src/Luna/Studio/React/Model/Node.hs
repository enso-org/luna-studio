{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Luna.Studio.React.Model.Node
    ( module Luna.Studio.React.Model.Node
    , Node.NodeId
    ) where

import           Control.Arrow
import           Data.Convert                         (Convertible (convert))
import           Data.Map.Lazy                        (Map)
import qualified Data.Map.Lazy                        as Map
import           Data.Position                        (Position, fromTuple)
import           Data.Time.Clock                      (UTCTime)

import           Empire.API.Data.MonadPath            (MonadPath)
import           Empire.API.Data.Node                 (NodeId, NodeType (InputEdge, OutputEdge))
import qualified Empire.API.Data.Node                 as Node
import qualified Empire.API.Data.NodeMeta             as NodeMeta
import           Empire.API.Data.Port                 (PortId, isArg, isInPort, isInPort, isOutPort, isProjection)
import           Empire.API.Graph.CollaborationUpdate (ClientId)
import           Empire.API.Graph.NodeResultUpdate    (NodeValue)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Port         (Port, fromPorts, portId)
import           Luna.Studio.State.Collaboration      (ColorId)


data Node = Node { _nodeId                :: NodeId
                 , _name                  :: Text
                 , _nodeType              :: NodeType
                 , _canEnter              :: Bool
                 , _ports                 :: Map PortId Port
                 , _position              :: Position
                 , _visualizationsEnabled :: Bool
                 , _code                  :: Maybe Text

                 , _expression            :: Text
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
countInPorts = foldl (\acc p -> acc + if isInPort $ p ^. portId then 1 else 0) 0 . getPorts

countOutPorts :: Node -> Int
countOutPorts = foldl (\acc p -> acc + if isOutPort $ p ^. portId then 1 else 0) 0 . getPorts

countArgPorts :: Node -> Int
countArgPorts = foldl (\acc p -> acc + if isArg $ p ^. portId then 1 else 0) 0 . getPorts

countProjectionPorts :: Node -> Int
countProjectionPorts = foldl (\acc p -> acc + if isProjection $ p ^. portId then 1 else 0) 0 . getPorts

makePorts :: Node.Node -> [Port]
makePorts node = fromPorts (node ^. Node.nodeId) (Map.elems $ node ^. Node.ports)

makePortsMap :: [Port] -> Map PortId Port
makePortsMap = Map.fromList . map (view portId &&& id)

instance Convertible Node.Node Node where
    convert n = Node
        {- nodeId                -} (n ^. Node.nodeId)
        {- name                  -} (n ^. Node.name)
        {- nodeType              -} (n ^. Node.nodeType)
        {- canEnter              -} (n ^. Node.canEnter)
        {- ports                 -} (makePortsMap $ makePorts n)
        {- position              -} (fromTuple $ n ^. Node.position)
        {- visualizationsEnabled -} (n ^. Node.nodeMeta . NodeMeta.displayResult)
        {- code                  -} (n ^. Node.code)

        {- expression            -} expression'
        {- value                 -} def
        {- zPos                  -} def
        {- isSelected            -} False
        {- mode                  -} def
        {- nameEdit              -} def
        {- execTime              -} def
        {- collaboration         -} def
        where
            expression' = case n ^. Node.nodeType of
                Node.ExpressionNode expr     -> expr
                Node.InputNode      inputIx  -> convert $ "Input " <> show inputIx
                Node.OutputNode     outputIx -> convert $ "Output " <> show outputIx
                Node.ModuleNode              -> "Module"
                Node.FunctionNode   _        -> "Function" -- & value .~ (convert $ intercalate " -> " tpeSig) --TODO[react]
                Node.InputEdge               -> "Input"
                Node.OutputEdge              -> "Output"
