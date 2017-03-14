module Empire.API.Data.Node where

import           Data.Binary              (Binary)
import           Data.Map.Lazy            (Map)
import           Data.Text                (Text)
import           Data.UUID.Types          (UUID)
import           Prologue

import           Empire.API.Data.NodeMeta (NodeMeta)
import qualified Empire.API.Data.NodeMeta as NodeMeta
import           Empire.API.Data.Port     (Port, PortId)


type NodeId = UUID

type FunctionType = [String]

data NodeType = ExpressionNode  { _expression :: Text }
              | InputEdge
              | OutputEdge
              | InputNode       { _inputIx    :: Int  }
              | OutputNode      { _outputIx   :: Int  }
              | FunctionNode    { _functionType :: FunctionType }
              | ModuleNode
              deriving (Generic, Eq, NFData, Show)

data Node = Node { _nodeId   :: NodeId
                 , _name     :: Text
                 , _nodeType :: NodeType
                 , _canEnter :: Bool
                 , _ports    :: Map PortId Port
                 , _nodeMeta :: NodeMeta
                 , _code     :: Maybe Text
                 } deriving (Generic, Eq, NFData, Show, Typeable)

data NodeTypecheckerUpdate = NodeTypecheckerUpdate {
      _tcNodeId :: NodeId
    , _tcPorts  :: Map PortId Port
    } deriving (Generic, Eq, NFData, Show, Typeable)

makeLenses ''Node
makeLenses ''NodeType
makePrisms ''NodeType
makeLenses ''NodeTypecheckerUpdate

position :: Lens' Node (Double, Double)
position = nodeMeta . NodeMeta.position

instance Binary Node
instance Binary NodeType
instance Binary NodeTypecheckerUpdate
