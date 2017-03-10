{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Luna.Studio.React.Model.Node
    ( module Luna.Studio.React.Model.Node
    , Node.NodeId
    ) where

import           Control.Arrow
import           Data.Map.Lazy                        (Map)
import qualified Data.Map.Lazy                        as Map
import           Data.Position                        (Position, fromTuple)
import           Data.Time.Clock                      (UTCTime)
import           Empire.API.Data.Node                 (NodeType (InputEdge, OutputEdge))
import qualified Empire.API.Data.Node                 as Node
import           Empire.API.Data.NodeMeta             (displayResult)
import           Empire.API.Data.Port                 (PortId, isArg, isInPort, isInPort, isOutPort, isProjection)
import           Empire.API.Graph.CollaborationUpdate (ClientId)
import           Empire.API.Graph.NodeResultUpdate    (NodeValue)
import           Luna.Studio.Prelude                  hiding (set)
import           Luna.Studio.React.Model.Port         (Port, fromPorts, portId)
import           Luna.Studio.State.Collaboration      (ColorId)


data Node = Node { _nodeId                :: Node.NodeId
                 , _ports                 :: Map PortId Port
                 , _position              :: Position
                 , _zPos                  :: Int
                 , _expression            :: Text
                 , _code                  :: Maybe Text
                 , _name                  :: Text
                 , _nameEdit              :: Maybe Text
                 , _value                 :: Maybe NodeValue
                 , _nodeType              :: NodeType
                 , _mode                  :: Mode
                 , _isSelected            :: Bool
                 , _visualizationsEnabled :: Bool
                 , _collaboration         :: Collaboration
                 , _execTime              :: Maybe Integer
                 } deriving (Eq, Generic, NFData, Show)

data Mode = Collapsed
          | Expanded
          | Editor
          deriving (Eq, Generic, NFData, Show)

data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, ColorId)
                                   , _modify :: Map ClientId  UTCTime
                                   } deriving (Default, Eq, Generic, NFData, Show)
makeLenses ''Node
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
isExpanded = isMode Expanded

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


makeNode :: Node.NodeId -> Map PortId Port -> Position -> Text -> Maybe Text -> Text -> NodeType -> Bool -> Node
makeNode nid ports' pos expr code' name' tpe' vis = Node nid ports' pos def expr code' name' def def tpe' def False vis def Nothing

makePorts :: Node.Node -> [Port]
makePorts node = fromPorts (node ^. Node.nodeId) (Map.elems $ node ^. Node.ports)

makePortsMap :: [Port] -> Map PortId Port
makePortsMap = Map.fromList . map (view portId &&& id)

fromNode :: Node.Node -> Node
fromNode n = makeNode nodeId' ports' position' expression' code' name' nodeType' vis where
    position'   = fromTuple $ n ^. Node.position
    nodeId'     = n ^. Node.nodeId
    name'       = n ^. Node.name
    vis         = n ^. Node.nodeMeta . displayResult
    code'       = n ^. Node.code
    nodeType'   = n ^. Node.nodeType
    ports'      = makePortsMap $ makePorts n
    expression' = case n ^. Node.nodeType of
        Node.ExpressionNode expr     -> expr
        Node.InputNode      inputIx  -> convert $ "Input " <> show inputIx
        Node.OutputNode     outputIx -> convert $ "Output " <> show outputIx
        Node.ModuleNode              -> "Module"
        Node.FunctionNode   _        -> "Function" -- & value .~ (convert $ intercalate " -> " tpeSig) --TODO[react]
        Node.InputEdge               -> "Input"
        Node.OutputEdge              -> "Output"
