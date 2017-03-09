{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Luna.Studio.React.Model.Node (
    module Luna.Studio.React.Model.Node,
    NodeAPI.NodeId
) where

import           Control.Arrow
import           Data.Map.Lazy                        (Map)
import qualified Data.Map.Lazy                        as Map
import           Data.Time.Clock                      (UTCTime)

import           Data.Position                        (Position (Position), Vector2 (Vector2))
import           Empire.API.Data.Node                 (NodeType)
import qualified Empire.API.Data.Node                 as NodeAPI
import qualified Empire.API.Data.NodeMeta             as MetaAPI
import           Empire.API.Data.Port                 (PortId (..))
import           Empire.API.Graph.CollaborationUpdate (ClientId)
import           Empire.API.Graph.NodeResultUpdate    (NodeValue)
import           Luna.Studio.Prelude                  hiding (set)
import           Luna.Studio.React.Model.Port         (Port)
import qualified Luna.Studio.React.Model.Port         as Port
import           Luna.Studio.State.Collaboration      (ColorId)


data Node = Node { _nodeId                :: NodeAPI.NodeId
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

isMode :: Mode -> Getter Node Bool
isMode mode' = to isMode' where
    isMode' node = node ^. mode == mode'

isExpanded :: Getter Node Bool
isExpanded = isMode Expanded

isCollapsed :: Getter Node Bool
isCollapsed = isMode Collapsed

isLiteral :: Getter Node Bool
isLiteral = to isLiteral' where
    isLiteral' node = not $ any isIn' portIds where
        portIds = Map.keys $ node ^. ports
        isIn' :: PortId -> Bool
        isIn' (OutPortId _) = False
        isIn' (InPortId  _) = True

makeNode :: NodeAPI.NodeId -> Map PortId Port -> Position -> Text -> Maybe Text -> Text -> NodeType -> Bool -> Node
makeNode nid ports' pos expr code' name' tpe' vis = Node nid ports' pos def expr code' name' def def tpe' def False vis def Nothing

makePorts :: NodeAPI.Node -> [Port]
makePorts node = Port.fromPorts (node ^. NodeAPI.nodeId) (Map.elems $ node ^. NodeAPI.ports)

fromNode :: NodeAPI.Node -> Node
fromNode n = makeNode nodeId' ports' position' expression' code' name' nodeType' vis where
    position' = Position (uncurry Vector2 $ n ^. NodeAPI.nodeMeta ^. MetaAPI.position)
    nodeId'     = n ^. NodeAPI.nodeId
    name'       = n ^. NodeAPI.name
    vis         = n ^. NodeAPI.nodeMeta . MetaAPI.displayResult
    code'       = n ^. NodeAPI.code
    nodeType'   = n ^. NodeAPI.nodeType
    ports'      = Map.fromList $ map (view Port.portId &&& id) $ makePorts n
    expression' = case n ^. NodeAPI.nodeType of
        NodeAPI.ExpressionNode expr     -> expr
        NodeAPI.InputNode      inputIx  -> convert $ "Input " <> show inputIx
        NodeAPI.OutputNode     outputIx -> convert $ "Output " <> show outputIx
        NodeAPI.ModuleNode              -> "Module"
        NodeAPI.FunctionNode _          -> "Function" -- & value .~ (convert $ intercalate " -> " tpeSig) --TODO[react]
        NodeAPI.InputEdge               -> "Input"
        NodeAPI.OutputEdge              -> "Output"


isEdge :: Node -> Bool
isEdge node = isInputEdge node || isOutputEdge node

isInputEdge :: Node -> Bool
isInputEdge node = node ^. nodeType == NodeAPI.InputEdge

isOutputEdge :: Node -> Bool
isOutputEdge node = node ^. nodeType == NodeAPI.OutputEdge
