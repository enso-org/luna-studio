module Empire.API.Data.Node where

import           Data.Binary               (Binary)
import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as Map
import           Data.UUID.Types           (UUID)
import           Data.Text.Lazy            (Text)
import           Prologue hiding (Text)

import           Empire.API.Data.NodeMeta  (NodeMeta)
import qualified Empire.API.Data.NodeMeta  as NodeMeta
import           Empire.API.Data.Port      (Port, PortId)
import qualified Empire.API.Data.Port      as Port


type NodeId = UUID

type FunctionType = [String]

data NodeType = ExpressionNode  { _expression :: Text }
              | InputEdge
              | OutputEdge
              | InputNode       { _inputIx    :: Int  }
              | OutputNode      { _outputIx   :: Int  }
              | FunctionNode    { _functionType :: FunctionType }
              | ModuleNode
              deriving (Show, Eq, Generic)

data Node = Node { _nodeId   :: NodeId
                 , _name     :: Text
                 , _nodeType :: NodeType
                 , _canEnter :: Bool
                 , _ports    :: Map PortId Port
                 , _nodeMeta :: NodeMeta
                 , _code     :: Maybe Text
                 } deriving (Generic, Typeable, Show, Eq)

makeLenses ''Node
makeLenses ''NodeType
makePrisms ''NodeType

position :: Lens' Node (Double, Double)
position = nodeMeta . NodeMeta.position

instance Binary Node
instance Binary NodeType
