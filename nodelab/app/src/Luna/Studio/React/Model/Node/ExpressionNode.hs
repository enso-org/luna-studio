{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node.ExpressionNode
    ( module Luna.Studio.React.Model.Node.ExpressionNode
    , module X
    , NodeId
    , NodeLoc
    ) where

import           Control.Arrow                         ((&&&))
import           Data.Convert                          (Convertible (convert))
import           Data.HashMap.Strict                   (HashMap)
import qualified Data.HashMap.Strict                   as HashMap
import           Data.Map.Lazy                         (Map)
import qualified Data.Map.Lazy                         as Map
import           Data.Position                         (Position, fromTuple, toTuple)
import           Data.Time.Clock                       (UTCTime)
import           Empire.API.Data.Breadcrumb            (BreadcrumbItem)
import           Empire.API.Data.MonadPath             (MonadPath)
import           Empire.API.Data.Node                  (NodeId)
import qualified Empire.API.Data.Node                  as Empire
import           Empire.API.Data.NodeLoc               (NodeLoc (NodeLoc), NodePath)
import qualified Empire.API.Data.NodeMeta              as NodeMeta
import           Empire.API.Graph.CollaborationUpdate  (ClientId)
import           Empire.API.Graph.NodeResultUpdate     (NodeValue(NodeError))
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.IsNode           as X (IsNode (..))
import           Luna.Studio.React.Model.Node.SidebarNode (SidebarNodesMap)
import           Luna.Studio.React.Model.Port             (PortsMap, isInPort)
import qualified Luna.Studio.React.Model.Port             as Port
import           Luna.Studio.State.Collaboration          (ColorId)


data ExpressionNode = ExpressionNode { _nodeLoc'              :: NodeLoc
                                     , _name                  :: Text
                                     , _expression            :: Text
                                     , _canEnter              :: Bool
                                     , _ports'                :: PortsMap
                                     , _position              :: Position
                                     , _visualizationsEnabled :: Bool
                                     , _code                  :: Maybe Text
                                     , _value                 :: Maybe NodeValue
                                     , _zPos                  :: Int
                                     , _isSelected            :: Bool
                                     , _mode                  :: Mode
                                     , _isNameEdited          :: Bool
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
  { _expressionNodes :: ExpressionNodesMap
  , _sidebarNodes    :: SidebarNodesMap
  , _monads          :: [MonadPath]
  } deriving (Default, Eq, Generic, NFData, Show)


data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, ColorId)
                                   , _modify :: Map ClientId  UTCTime
                                   } deriving (Default, Eq, Generic, NFData, Show)

type ExpressionNodesMap = HashMap NodeId ExpressionNode

makeLenses ''Collaboration
makeLenses ''ExpressionNode
makeLenses ''Subgraph
makePrisms ''ExpandedMode
makePrisms ''Mode

instance Convertible (NodePath, Empire.Node, Text) ExpressionNode where
    convert (path, n, expr) = ExpressionNode
        {- nodeLoc               -} (NodeLoc path $ n ^. Empire.nodeId)
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
        {- isNameEdited          -} False
        {- execTime              -} def
        {- collaboration         -} def

instance Convertible ExpressionNode Empire.Node where
    convert n = Empire.Node
        {- nodeId   -} (n ^. nodeId)
        {- name     -} (n ^. name)
        {- nodeType -} (Empire.ExpressionNode $ n ^. expression)
        {- canEnter -} (n ^. canEnter)
        {- ports    -} (convert <$> n ^. ports)
        {- nodeMeta -} (NodeMeta.NodeMeta (toTuple $ n ^. position) (n ^. visualizationsEnabled))
        {- code     -} (n ^. code)


instance Default Mode where def = Collapsed

instance IsNode ExpressionNode where
    nodeLoc              = nodeLoc'
    ports                = ports'
    getPorts             = Map.elems . view ports
    hasPort pid          = Map.member pid . view ports
    countInPorts         = Port.countInPorts . Map.keys . (view ports)
    countOutPorts        = Port.countOutPorts . Map.keys . (view ports)
    countArgPorts        = Port.countArgPorts . Map.keys . (view ports)
    countProjectionPorts = Port.countProjectionPorts . Map.keys . (view ports)

toExpressionNodesMap :: [ExpressionNode] -> ExpressionNodesMap
toExpressionNodesMap = HashMap.fromList . map (view nodeId &&& id)

subgraphs :: Applicative f => (Map BreadcrumbItem Subgraph -> f (Map BreadcrumbItem Subgraph)) -> ExpressionNode -> f ExpressionNode
subgraphs = mode . _Expanded . _Function

returnsError :: ExpressionNode -> Bool
returnsError node = case node ^. value of
    Just (NodeError _) -> True
    _                  -> False

isMode :: Mode -> ExpressionNode -> Bool
isMode mode' node = node ^. mode == mode'

isExpanded :: ExpressionNode -> Bool
isExpanded node = case node ^. mode of
    Expanded _ -> True
    _          -> False

isExpandedControls :: ExpressionNode -> Bool
isExpandedControls = isMode (Expanded Controls)

isExpandedFunction :: ExpressionNode -> Bool
isExpandedFunction node = case node ^. mode of
    Expanded (Function _) -> True
    _                     -> False

isCollapsed :: ExpressionNode -> Bool
isCollapsed = isMode Collapsed

isLiteral :: ExpressionNode -> Bool
isLiteral node = not $ any isInPort portIds where
    portIds = Map.keys $ node ^. ports
