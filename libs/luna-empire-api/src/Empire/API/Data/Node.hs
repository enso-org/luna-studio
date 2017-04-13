{-# LANGUAGE Rank2Types #-}
module Empire.API.Data.Node where

import           Control.Arrow            ((&&&))
import           Data.Binary              (Binary)
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
import           Data.Maybe               (isJust)
import           Data.Text                (Text)
import           Data.UUID.Types          (UUID)
import           Empire.API.Data.NodeMeta (NodeMeta)
import qualified Empire.API.Data.NodeMeta as NodeMeta
import           Empire.API.Data.Port     (OutPortTree, Port, PortId)
import qualified Empire.API.Data.Port     as Port
import           Prologue


type NodeId = UUID

data NodeType = ExpressionNode { _expression     :: Text }
              | InputEdge      { _inputEdgePorts :: [OutPortTree Port] }
              | OutputEdge
              deriving (Generic, Eq, NFData, Show)

data Node = Node { _nodeId   :: NodeId
                 , _name     :: Text
                 , _nodeType :: NodeType
                 , _canEnter :: Bool
                 , _ports    :: Map PortId Port -- FIXME[LJK, MK]: At some point we need to drop this map and unify all port types across the project
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

isEdge :: Node -> Bool
isEdge node = isInputEdge node || isOutputEdge node

isInputEdge :: Node -> Bool
isInputEdge node = isJust $ node ^? nodeType . _InputEdge

isOutputEdge :: Node -> Bool
isOutputEdge node = node ^. nodeType == OutputEdge

makePortsMap :: [Port] -> Map PortId Port
makePortsMap = Map.fromList . map (view Port.portId &&& id)
