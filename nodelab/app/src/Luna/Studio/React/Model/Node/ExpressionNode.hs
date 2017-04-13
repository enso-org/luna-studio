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

import           Data.Convert                             (Convertible (convert))
import           Data.HashMap.Strict                      (HashMap)
import           Data.Map.Lazy                            (Map)
import           Data.Position                            (Position, fromTuple, toTuple)
import           Data.Time.Clock                          (UTCTime)
import           Empire.API.Data.Breadcrumb               (BreadcrumbItem)
import           Empire.API.Data.MonadPath                (MonadPath)
import           Empire.API.Data.Node                     (NodeId)
import qualified Empire.API.Data.Node                     as Empire
import           Empire.API.Data.NodeLoc                  (NodeLoc (NodeLoc), NodePath)
import qualified Empire.API.Data.NodeMeta                 as NodeMeta
import           Empire.API.Graph.CollaborationUpdate     (ClientId)
import           Empire.API.Graph.NodeResultUpdate        (NodeValue, NodeValue (Error))
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.IsNode           as X
import           Luna.Studio.React.Model.Node.SidebarNode (InputNode, OutputNode)
import           Luna.Studio.React.Model.Port             (InPort, InPortTree, OutPort, OutPortTree)
import qualified Luna.Studio.React.Model.Port             as Port
import           Luna.Studio.State.Collaboration          (ColorId)


data ExpressionNode = ExpressionNode { _nodeLoc'              :: NodeLoc
                                     , _name                  :: Maybe Text
                                     , _expression            :: Text
                                     , _canEnter              :: Bool
                                     , _inPorts               :: InPortTree InPort
                                     , _outPorts              :: OutPortTree OutPort
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
        , _inputNode       :: Maybe InputNode
        , _outputNode      :: Maybe OutputNode
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

instance Convertible (NodePath, Empire.ExpressionNode) ExpressionNode where
    convert (path, n) = ExpressionNode
        {- nodeLoc               -} (NodeLoc path $ n ^. Empire.nodeId)
        {- name                  -} (n ^. Empire.name)
        {- expression            -} (n ^. Empire.expression)
        {- canEnter              -} (n ^. Empire.canEnter)
        {- inPorts               -} (convert <$> n ^. Empire.inPorts)
        {- outPorts              -} (convert <$> n ^. Empire.outPorts)
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

instance Convertible ExpressionNode Empire.ExpressionNode where
    convert n = Empire.ExpressionNode
        {- exprNodeId -} (n ^. nodeId)
        {- expression -} (n ^. expression)
        {- name       -} (n ^. name)
        {- code       -} (n ^. code)
        {- inPorts    -} (convert <$> n ^. inPorts)
        {- outPorts   -} (convert <$> n ^. outPorts)
        {- nodeMeta   -} (NodeMeta.NodeMeta (toTuple $ n ^. position) (n ^. visualizationsEnabled))
        {- canEnter   -} (n ^. canEnter)

instance Default Mode where def = Collapsed

instance HasNodeLoc ExpressionNode where
    nodeLoc = nodeLoc'

instance HasPorts ExpressionNode where
    inPortsList = Port.inPortTreeLeafs . view inPorts
    outPortsList = Port.outPortTreeLeafs . view outPorts
    inPortAt  pid = inPorts . ix pid
    outPortAt pid = outPorts . ix pid

subgraphs :: Applicative f => (Map BreadcrumbItem Subgraph -> f (Map BreadcrumbItem Subgraph)) -> ExpressionNode -> f ExpressionNode
subgraphs = mode . _Expanded . _Function

returnsError :: ExpressionNode -> Bool
returnsError node = case node ^. value of
    Just (Error _) -> True
    _              -> False

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
