{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module NodeEditor.React.Model.Node.ExpressionNode
    ( module NodeEditor.React.Model.Node.ExpressionNode
    , module X
    , NodeId
    , NodeLoc
    ) where

import           Common.Prelude
import           Data.Convert                             (Convertible (convert))
import           Data.HashMap.Strict                      (HashMap)
import           Data.Map.Lazy                            (Map)
import           Data.Time.Clock                          (UTCTime)
import           LunaStudio.API.Graph.CollaborationUpdate (ClientId)
import           LunaStudio.Data.Breadcrumb               (BreadcrumbItem)
import           LunaStudio.Data.Error                    (Error)
import           LunaStudio.Data.MonadPath                (MonadPath)
import           LunaStudio.Data.Node                     (NodeId)
import qualified LunaStudio.Data.Node                     as Empire
import           LunaStudio.Data.NodeLoc                  (NodeLoc (NodeLoc), NodePath)
import qualified LunaStudio.Data.NodeLoc                  as NodeLoc
import qualified LunaStudio.Data.NodeMeta                 as NodeMeta
import           LunaStudio.Data.NodeValue                (ShortValue, VisualizationId, Visualizer, VisualizerName, VisualizerPath)
import           LunaStudio.Data.Position                 (Position)
import           NodeEditor.React.Model.IsNode            as X
import           NodeEditor.React.Model.Node.SidebarNode  (InputNode, OutputNode)
import           NodeEditor.React.Model.Port              (InPort, InPortTree, OutPort, OutPortTree)
import qualified NodeEditor.React.Model.Port              as Port
import           NodeEditor.State.Collaboration           (ColorId)


data ExpressionNode = ExpressionNode { _nodeLoc'                :: NodeLoc
                                     , _name                    :: Maybe Text
                                     , _expression              :: Text
                                     , _canEnter                :: Bool
                                     , _inPorts                 :: InPortTree InPort
                                     , _outPorts                :: OutPortTree OutPort
                                     , _position                :: Position
                                     , _visualizationsEnabled   :: Bool
                                     , _code                    :: Maybe Text
                                     , _value                   :: Maybe Value
                                     , _activeVisualizationInfo :: Maybe (Maybe VisualizationId, Visualizer)
                                     , _visualizers             :: Map VisualizerName VisualizerPath
                                     , _zPos                    :: Int
                                     , _isSelected              :: Bool
                                     , _mode                    :: Mode
                                     , _execTime                :: Maybe Integer
                                     , _collaboration           :: Collaboration
                                     } deriving (Eq, Generic, NFData, Show)


data Mode = Collapsed
          | Expanded ExpandedMode
          deriving (Eq, Generic, NFData, Show)

data ExpandedMode = Editor
                  | Controls
                  | Function (Map BreadcrumbItem Subgraph)
                  deriving (Eq, Generic, NFData, Show)

data Subgraph = Subgraph { _expressionNodes :: ExpressionNodesMap
                         , _inputNode       :: Maybe InputNode
                         , _outputNode      :: Maybe OutputNode
                         , _monads          :: [MonadPath]
                         } deriving (Default, Eq, Generic, NFData, Show)

data Value = ShortValue ShortValue
           | Error      Error
           deriving (Eq, Generic, NFData, Show)

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
        {- nodeLoc                 -} (NodeLoc path $ n ^. Empire.nodeId)
        {- name                    -} (n ^. Empire.name)
        {- expression              -} (n ^. Empire.expression)
        {- canEnter                -} (n ^. Empire.canEnter)
        {- inPorts                 -} (convert <$> n ^. Empire.inPorts)
        {- outPorts                -} (convert <$> n ^. Empire.outPorts)
        {- position                -} (n ^. Empire.position)
        {- visualizationsEnabled   -} (n ^. Empire.nodeMeta . NodeMeta.displayResult)
        {- code                    -} (n ^. Empire.code)
        {- value                   -} def
        {- activeVisualizationInfo -} def
        {- visualizers             -} def
        {- zPos                    -} def
        {- isSelected              -} False
        {- mode                    -} def
        {- execTime                -} def
        {- collaboration           -} def

instance Convertible ExpressionNode Empire.ExpressionNode where
    convert n = Empire.ExpressionNode
        {- exprNodeId -} (n ^. nodeId)
        {- expression -} (n ^. expression)
        {- name       -} (n ^. name)
        {- code       -} (n ^. code)
        {- inPorts    -} (convert <$> n ^. inPorts)
        {- outPorts   -} (convert <$> n ^. outPorts)
        {- nodeMeta   -} (NodeMeta.NodeMeta (n ^. position) (n ^. visualizationsEnabled))
        {- canEnter   -} (n ^. canEnter)

instance Default Mode where def = Collapsed

instance HasNodeLoc ExpressionNode where
    nodeLoc = nodeLoc'

instance HasPorts ExpressionNode where
    inPortsList = Port.inPortTreeLeafs . view inPorts
    outPortsList = Port.outPortTreeLeafs . view outPorts
    inPortAt  pid = inPorts . ix pid
    outPortAt pid = outPorts . ix pid

mkExprNode :: NodeLoc -> Text -> Position -> ExpressionNode
mkExprNode nl expr pos = convert (nl ^. NodeLoc.path, Empire.mkExprNode (nl ^. NodeLoc.nodeId) expr pos)

subgraphs :: Applicative f => (Map BreadcrumbItem Subgraph -> f (Map BreadcrumbItem Subgraph)) -> ExpressionNode -> f ExpressionNode
subgraphs = mode . _Expanded . _Function

returnsError :: ExpressionNode -> Bool
returnsError node = case node ^. value of
    Just (Error _) -> True
    _              -> False

getActiveVisualization :: ExpressionNode -> Maybe (VisualizationId, Visualizer)
getActiveVisualization n = case n ^. activeVisualizationInfo of
    Just (Just vid, vis) -> Just (vid, vis)
    _                    -> Nothing

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

findPredecessorPosition :: ExpressionNode -> [ExpressionNode] -> Position
findPredecessorPosition n nodes = Empire.findPredecessorPosition (convert n) $ map convert nodes

findSuccessorPosition :: ExpressionNode -> [ExpressionNode] -> Position
findSuccessorPosition n nodes = Empire.findSuccessorPosition (convert n) $ map convert nodes
