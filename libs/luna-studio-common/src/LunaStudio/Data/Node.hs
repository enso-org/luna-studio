module LunaStudio.Data.Node
    ( module LunaStudio.Data.Node
    , module X
    ) where

import           Control.Arrow             ((&&&))
import           Control.Lens              (makePrisms)
import           Data.Aeson.Types          (FromJSON, ToJSON)
import           Data.Binary               (Binary)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           LunaStudio.Data.Constants (gapBetweenNodes)
import           LunaStudio.Data.NodeId    as X
import           LunaStudio.Data.NodeLoc   (NodeLoc)
import           LunaStudio.Data.NodeMeta  (NodeMeta)
import qualified LunaStudio.Data.NodeMeta  as NodeMeta
import           LunaStudio.Data.Port      (InPort, InPortTree, OutPort, OutPortTree)
import qualified LunaStudio.Data.Port      as Port
import           LunaStudio.Data.Position  (Position, fromDoubles, x, y)
import           LunaStudio.Data.TypeRep   (TypeRep (TStar))
import           Prologue


data Node
    = ExpressionNode' ExpressionNode
    | InputSidebar' InputSidebar
    | OutputSidebar' OutputSidebar
    deriving (Eq, Generic, Show, Typeable)

data ExpressionNode = ExpressionNode
    { _exprNodeId       :: NodeId
    , _expression       :: Text
    , _isDefinition     :: Bool
    , _name             :: Maybe Text
    , _code             :: Text
    , _inPorts          :: InPortTree  InPort
    , _outPorts         :: OutPortTree OutPort
    , _nodeMeta         :: NodeMeta
    , _canEnter         :: Bool
    } deriving (Eq, Generic, Show, Typeable)

data InputSidebar = InputSidebar
    { _inputNodeId    :: NodeId
    , _inputEdgePorts :: [OutPortTree OutPort]
    , _isDef          :: Bool
    } deriving (Eq, Generic, Show, Typeable)

data OutputSidebar = OutputSidebar
    { _outputNodeId    :: NodeId
    , _outputEdgePorts :: InPortTree InPort
    } deriving (Eq, Generic, Show, Typeable)

--FIXME: Invalid lenses!!!
data NodeTypecheckerUpdate
    = ExpressionUpdate    { _tcNodeId   :: NodeId, _tcInPorts       :: InPortTree InPort     , _tcOutPorts :: OutPortTree OutPort }
    | OutputSidebarUpdate { _tcNodeId   :: NodeId, _tcInPorts       :: InPortTree InPort     }
    | InputSidebarUpdate  { _tcNodeId   :: NodeId, _tcInputOutPorts :: [OutPortTree OutPort] }
    deriving (Eq, Generic, Show, Typeable)

makePrisms ''Node
makeLenses ''ExpressionNode
makeLenses ''InputSidebar
makeLenses ''OutputSidebar
makeLenses ''NodeTypecheckerUpdate

position :: Lens' ExpressionNode Position
position = nodeMeta . NodeMeta.position


instance Binary   ExpressionNode
instance NFData   ExpressionNode
instance FromJSON ExpressionNode
instance ToJSON   ExpressionNode
instance Binary   InputSidebar
instance NFData   InputSidebar
instance FromJSON InputSidebar
instance ToJSON   InputSidebar
instance Binary   OutputSidebar
instance NFData   OutputSidebar
instance FromJSON OutputSidebar
instance ToJSON   OutputSidebar
instance Binary   NodeTypecheckerUpdate
instance NFData   NodeTypecheckerUpdate
instance FromJSON NodeTypecheckerUpdate
instance ToJSON   NodeTypecheckerUpdate
instance Binary   Node
instance NFData   Node
instance FromJSON Node
instance ToJSON   Node

class HasNodeId a where nodeId :: Lens' a NodeId

instance HasNodeId ExpressionNode where nodeId = exprNodeId
instance HasNodeId InputSidebar where nodeId = inputNodeId
instance HasNodeId OutputSidebar where nodeId = outputNodeId
instance HasNodeId Node where
    nodeId f (ExpressionNode' node) = ExpressionNode' <$> nodeId f node
    nodeId f (InputSidebar'   node) = InputSidebar'   <$> nodeId f node
    nodeId f (OutputSidebar'  node) = OutputSidebar'  <$> nodeId f node

mkExprNode :: NodeId -> Text -> Position -> ExpressionNode
mkExprNode nid expr pos = ExpressionNode
    nid
    expr
    False
    def
    def
    (Port.LabeledTree
        (Port.InPorts
            (Just $ Port.LabeledTree def $ Port.Port
                [Port.Self]
                (Text.pack "")
                TStar
                Port.NotConnected
            )
            def
            def
        )
        (Port.Port [] (Text.pack "") TStar Port.NotConnected)
    )
    (Port.LabeledTree def $ Port.Port [] (Text.pack "") TStar Port.NotConnected)
    (NodeMeta.NodeMeta pos False def)
    False

findPredecessorPosition :: ExpressionNode -> [ExpressionNode] -> Position
findPredecessorPosition node nodes = fromDoubles xPos yPos where
    xPos = (node ^. position . x) - gapBetweenNodes
    yPos = findYPos $ node ^. position . y
    inCandidatePosition y' n
        = n ^. position . x == xPos && n ^. position . y == y'
    findYPos y' = if any (inCandidatePosition y') nodes
        then findYPos $ y' - gapBetweenNodes
        else y'

findSuccessorPosition :: ExpressionNode -> [ExpressionNode] -> Position
findSuccessorPosition node nodes = fromDoubles xPos yPos where
    xPos = (node ^. position . x) + gapBetweenNodes
    yPos = findYPos $ node ^. position . y
    inCandidatePosition y' n
        = n ^. position . x == xPos && n ^. position . y == y'
    findYPos y' = if any (inCandidatePosition y') nodes
        then findYPos $ y' + gapBetweenNodes
        else y'


toExpressionNodesMap :: [ExpressionNode] -> Map NodeLoc ExpressionNode
toExpressionNodesMap = fromList . fmap (convert . view nodeId &&& id)
