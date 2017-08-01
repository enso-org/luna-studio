{-# LANGUAGE Rank2Types #-}
module LunaStudio.Data.Node where

import           Data.Binary               (Binary)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.UUID.Types           (UUID)
import           LunaStudio.Data.Constants (gapBetweenNodes)
import           LunaStudio.Data.NodeMeta  (NodeMeta)
import qualified LunaStudio.Data.NodeMeta  as NodeMeta
import           LunaStudio.Data.Port      (InPort, InPortTree, OutPort, OutPortTree)
import qualified LunaStudio.Data.Port      as Port
import           LunaStudio.Data.Position  (Position, fromDoubles, x, y)
import           LunaStudio.Data.TypeRep   (TypeRep (TStar))
import           Prologue


type NodeId = UUID

data Node = ExpressionNode' ExpressionNode | InputSidebar' InputSidebar | OutputSidebar' OutputSidebar deriving (Eq, Generic, Show, Typeable)

data ExpressionNode = ExpressionNode { _exprNodeId       :: NodeId
                                     , _expression       :: Text
                                     , _isDefinition     :: Bool
                                     , _name             :: Maybe Text
                                     , _code             :: Text
                                     , _inPorts          :: InPortTree  InPort
                                     , _outPorts         :: OutPortTree OutPort
                                     , _nodeMeta         :: NodeMeta
                                     , _canEnter         :: Bool
                                     } deriving (Eq, Generic, Show, Typeable)

data InputSidebar = InputSidebar { _inputNodeId    :: NodeId
                                 , _inputEdgePorts :: [OutPortTree OutPort]
                                 } deriving (Eq, Generic, Show, Typeable)

data OutputSidebar = OutputSidebar { _outputNodeId    :: NodeId
                                   , _outputEdgePorts :: InPortTree InPort
                                   } deriving (Eq, Generic, Show, Typeable)

data NodeTypecheckerUpdate = ExpressionUpdate    { _tcNodeId   :: NodeId, _tcInPorts       :: InPortTree InPort, _tcOutPorts :: OutPortTree OutPort }
                           | OutputSidebarUpdate { _tcNodeId   :: NodeId, _tcInPorts       :: InPortTree InPort }
                           | InputSidebarUpdate  { _tcNodeId   :: NodeId, _tcInputOutPorts :: [OutPortTree OutPort] }
                           deriving (Eq, Generic, Show, Typeable)

makeLenses ''ExpressionNode
makeLenses ''InputSidebar
makeLenses ''OutputSidebar
makeLenses ''NodeTypecheckerUpdate

position :: Lens' ExpressionNode Position
position = nodeMeta . NodeMeta.position

instance Binary ExpressionNode
instance NFData ExpressionNode
instance Binary InputSidebar
instance NFData InputSidebar
instance Binary OutputSidebar
instance NFData OutputSidebar
instance Binary NodeTypecheckerUpdate
instance NFData NodeTypecheckerUpdate
instance Binary Node
instance NFData Node

class HasNodeId a where nodeId :: Lens' a NodeId

instance HasNodeId ExpressionNode where nodeId = exprNodeId
instance HasNodeId InputSidebar where nodeId = inputNodeId
instance HasNodeId OutputSidebar where nodeId = outputNodeId
instance HasNodeId Node where
    nodeId f (ExpressionNode' node) = ExpressionNode' <$> nodeId f node
    nodeId f (InputSidebar'   node) = InputSidebar'   <$> nodeId f node
    nodeId f (OutputSidebar'  node) = OutputSidebar'  <$> nodeId f node

mkExprNode :: NodeId -> Text -> Position -> ExpressionNode
mkExprNode nid expr pos = ExpressionNode nid
                                         expr
                                         False
                                         def
                                         def
                                         (Port.LabeledTree (Port.InPorts (Just $ Port.LabeledTree def $ Port.Port [Port.Self] (Text.pack "") TStar Port.NotConnected) def def) (Port.Port [] (Text.pack "") TStar Port.NotConnected))
                                         (Port.LabeledTree def $ Port.Port []          (Text.pack "") TStar Port.NotConnected)
                                         (NodeMeta.NodeMeta pos False def)
                                         False

findPredecessorPosition :: ExpressionNode -> [ExpressionNode] -> Position
findPredecessorPosition node nodes = fromDoubles xPos yPos where
    xPos = (node ^. position . x) - gapBetweenNodes
    yPos = findYPos $ node ^. position . y
    findYPos y' = if any (\n -> n ^. position . x == xPos && n ^. position . y == y') nodes then findYPos $ y' - gapBetweenNodes else y'

findSuccessorPosition :: ExpressionNode -> [ExpressionNode] -> Position
findSuccessorPosition node nodes = fromDoubles xPos yPos where
    xPos = (node ^. position . x) + gapBetweenNodes
    yPos = findYPos $ node ^. position . y
    findYPos y' = if any (\n -> n ^. position . x == xPos && n ^. position . y == y') nodes then findYPos $ y' + gapBetweenNodes else y'
