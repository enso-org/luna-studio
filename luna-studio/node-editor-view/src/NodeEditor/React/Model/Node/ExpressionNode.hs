{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE StrictData        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module NodeEditor.React.Model.Node.ExpressionNode (module NodeEditor.React.Model.Node.ExpressionNode, module X) where

import LunaStudio.Data.Node          as X (NodeId)
import LunaStudio.Data.NodeLoc       as X (NodeLoc)
import NodeEditor.React.Model.IsNode as X

import Common.Prelude

import qualified LunaStudio.Data.Node        as Empire
import qualified LunaStudio.Data.NodeLoc     as NodeLoc
import qualified LunaStudio.Data.NodeMeta    as NodeMeta
import qualified LunaStudio.Data.PortRef     as PortRef
import qualified NodeEditor.React.Model.Port as Port
import qualified LunaStudio.Data.Project     as Project

import Data.Convert                             (Convertible (convert))
import Data.HashMap.Strict                      (HashMap)
import Data.Map.Lazy                            (Map)
import Data.Time.Clock                          (UTCTime)
import LunaStudio.API.Graph.CollaborationUpdate (ClientId)
import LunaStudio.Data.Breadcrumb               (BreadcrumbItem)
import LunaStudio.Data.Error                    (Error, NodeError)
import LunaStudio.Data.MonadPath                (MonadPath)
import LunaStudio.Data.NodeLoc                  (NodeLoc (NodeLoc), NodePath)
import LunaStudio.Data.NodeMeta                 (NodeMeta (NodeMeta))
import LunaStudio.Data.NodeValue                (ShortValue)
import LunaStudio.Data.Position                 (Position, move)
import LunaStudio.Data.TypeRep                  (TypeRep)
import LunaStudio.Data.Vector2                  (Vector2 (Vector2))
import NodeEditor.Data.Color                    (Color)
import NodeEditor.React.Model.Constants         (nodeRadius)
import NodeEditor.React.Model.Node.SidebarNode  (InputNode, OutputNode)
import NodeEditor.React.Model.Port              (AnyPortId (InPortId', OutPortId'), InPort, InPortId, InPortTree, OutPort, OutPortId, OutPortTree)
import NodeEditor.React.Model.Visualization     (Visualizer)


data ExpressionNode = ExpressionNode { _nodeLoc'                  :: NodeLoc
                                     , _name                      :: Maybe Text
                                     , _expression                :: Text
                                     , _isDefinition              :: Bool
                                     , _inPorts                   :: InPortTree InPort
                                     , _outPorts                  :: OutPortTree OutPort
                                     , _argConstructorMode        :: Port.Mode
                                     , _canEnter                  :: Bool
                                     , _position                  :: Position
                                     , _defaultVisualizer         :: Maybe Visualizer
                                     , _visEnabled                :: Bool
                                     , _errorVisEnabled           :: Bool
                                     , _code                      :: Text
                                     , _value                     :: Value
                                     , _zPos                      :: Int
                                     , _isSelected                :: Bool
                                     , _isMouseOver               :: Bool
                                     , _mode                      :: Mode
                                     , _execTime                  :: Maybe Integer
                                     , _collaboration             :: Collaboration
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

data Value = AwaitingTypecheck
           | AwaitingData
           | ShortValue ShortValue
           | Error      (Error NodeError)
           deriving (Eq, Generic, NFData, Show)

instance Default Value where def = AwaitingTypecheck

data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, Color)
                                   , _modify :: Map ClientId  UTCTime
                                   } deriving (Default, Eq, Generic, NFData, Show)

type ExpressionNodesMap = HashMap NodeId ExpressionNode

makeLenses ''Collaboration
makeLenses ''ExpressionNode
makeLenses ''Subgraph
makePrisms ''ExpandedMode
makePrisms ''Mode
makePrisms ''Value

instance Convertible (NodePath, Empire.ExpressionNode) ExpressionNode where
    convert (path, n) = ExpressionNode
        {- nodeLoc                   -} (NodeLoc path $ n ^. Empire.nodeId)
        {- name                      -} (n ^. Empire.name)
        {- expression                -} (n ^. Empire.expression)
        {- isDefinition              -} (n ^. Empire.isDefinition)
        {- inPorts                   -} (convert <$> n ^. Empire.inPorts)
        {- outPorts                  -} (convert <$> n ^. Empire.outPorts)
        {- argConstructorHighlighted -} Port.Invisible
        {- canEnter                  -} (n ^. Empire.canEnter)
        {- position                  -} (n ^. Empire.position)
        {- defaultVisualizer         -} (Project.fromOldAPI <$> n ^. Empire.nodeMeta . NodeMeta.selectedVisualizer)
        {- visEnabled                -} (n ^. Empire.nodeMeta . NodeMeta.displayResult)
        {- errorVisEnabled           -} False
        {- code                      -} (n ^. Empire.code)
        {- value                     -} def
        {- zPos                      -} def
        {- isSelected                -} False
        {- isMouseOver               -} False
        {- mode                      -} def
        {- execTime                  -} def
        {- collaboration             -} def

instance Convertible ExpressionNode Empire.ExpressionNode where
    convert n = Empire.ExpressionNode
        {- exprNodeId   -} (n ^. nodeId)
        {- expression   -} (n ^. expression)
        {- isDefinition -} (n ^. isDefinition)
        {- name         -} (n ^. name)
        {- code         -} (n ^. code)
        {- inPorts      -} (convert <$> n ^. inPorts)
        {- outPorts     -} (convert <$> n ^. outPorts)
        {- nodeMeta     -} (NodeMeta.NodeMeta (n ^. position) (n ^. visEnabled) (Project.toOldAPI <$> n ^. defaultVisualizer))
        {- canEnter     -} (n ^. canEnter)

instance Default Mode where def = Collapsed

instance HasNodeLoc ExpressionNode where
    nodeLoc = nodeLoc'

instance HasPorts ExpressionNode where
    inPortsList    = Port.visibleInPorts . view inPorts
    outPortsList   = Port.visibleOutPorts . view outPorts
    inPortAt   pid = inPorts . ix pid
    outPortAt  pid = outPorts . ix pid
    portModeAt pid = \f n -> if (PortRef.InPortRef' $ argumentConstructorRef n) ^. PortRef.portId == pid
        then argConstructorMode f n
        else case pid of
            OutPortId' outpid -> (outPortAt outpid . Port.mode) f n
            InPortId'  inpid  -> (inPortAt  inpid  . Port.mode) f n

--TODO[LJK, JK]: return precise value here
toNodeTopPosition :: Position -> Position
toNodeTopPosition = move (Vector2 0 (-2 * nodeRadius))

topPosition :: Getter ExpressionNode Position
topPosition = to $ toNodeTopPosition . view position

mkExprNode :: NodeLoc -> Text -> Position -> ExpressionNode
mkExprNode nl expr pos = convert (nl ^. NodeLoc.path, Empire.mkExprNode (nl ^. NodeLoc.nodeId) expr pos)

subgraphs :: Applicative f => (Map BreadcrumbItem Subgraph -> f (Map BreadcrumbItem Subgraph)) -> ExpressionNode -> f ExpressionNode
subgraphs = mode . _Expanded . _Function

returnsError :: ExpressionNode -> Bool
returnsError node = case node ^. value of
    Error _ -> True
    _       -> False

hasData :: ExpressionNode -> Bool
hasData node = case node ^. value of
    ShortValue {} -> True
    Error      {} -> True
    _             -> False

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

nodeType :: Getter ExpressionNode (Maybe TypeRep)
nodeType = to (^? outPortAt [] . Port.valueType)

visualizationsEnabled :: Lens' ExpressionNode Bool
visualizationsEnabled = lens getVisualizationEnabled setVisualizationEnabled where
    getVisualizationEnabled n   = if returnsError n then n ^. errorVisEnabled     else n ^. visEnabled
    setVisualizationEnabled n v = if returnsError n then n & errorVisEnabled .~ v else n & visEnabled .~ v

nodeMeta :: Lens' ExpressionNode NodeMeta
nodeMeta = lens getNodeMeta setNodeMeta where
    getNodeMeta n    = NodeMeta (n ^. position) (n ^. visEnabled) (Project.toOldAPI <$> n ^. defaultVisualizer)
    setNodeMeta n nm = n & position              .~ nm ^. NodeMeta.position
                         & visEnabled            .~ nm ^. NodeMeta.displayResult
                         & defaultVisualizer     .~ (Project.fromOldAPI <$> nm ^. NodeMeta.selectedVisualizer)

containsNode :: NodeLoc -> NodeLoc -> Bool
containsNode nl nlToCheck = inSubgraph False $ NodeLoc.toNodeIdList nl where
    parentNid  = nl ^. NodeLoc.nodeId
    nidToCheck = nlToCheck ^. NodeLoc.nodeId
    inSubgraph _                []           = False
    inSubgraph parentNidVisited (nid : nids) = do
        let visited = parentNidVisited || parentNid == nid
        if visited && nidToCheck == nid then True else inSubgraph visited nids

isAnyPortHighlighted :: ExpressionNode -> Bool
isAnyPortHighlighted n = (any Port.isHighlighted $ inPortsList n)
                      || (any Port.isHighlighted $ outPortsList n)
                      || Port.Highlighted == n ^. argConstructorMode


visibleOutPortNumber :: ExpressionNode -> OutPortId -> Int
visibleOutPortNumber n pid = fromMaybe def $ findIndex (\p -> p ^. Port.portId == pid) . Port.visibleOutPorts $ n ^. outPorts

visibleInPortNumber :: ExpressionNode -> InPortId -> Int
visibleInPortNumber n pid = fromMaybe def $ findIndex (\p -> p ^. Port.portId == pid) . Port.visibleInPorts $ n ^. inPorts

visibleArgPortNumber :: ExpressionNode -> InPortId -> Int
visibleArgPortNumber n pid = fromMaybe def $ findIndex (\p -> p ^. Port.portId == pid) . filter (Port.isArg . view Port.portId) . Port.visibleInPorts $ n ^. inPorts

countVisibleOutPorts :: ExpressionNode -> Int
countVisibleOutPorts n = length . Port.visibleOutPorts $ n ^. outPorts

countVisibleInPorts :: ExpressionNode -> Int
countVisibleInPorts n = length . Port.visibleInPorts $ n ^. inPorts

countVisibleArgPorts :: ExpressionNode -> Int
countVisibleArgPorts n = length . filter (Port.isArg . view Port.portId) . Port.visibleInPorts $ n ^. inPorts
