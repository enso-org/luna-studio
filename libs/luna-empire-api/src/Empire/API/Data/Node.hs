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
import           Empire.API.Data.Port     (OutPortTree, Port, PortId, InPort)
import qualified Empire.API.Data.Port     as Port
import           Prologue


type NodeId = UUID

data ExpressionNode = ExpressionNode { _exprNodeId :: NodeId
                                     , _expression :: Text
                                     , _name       :: Maybe Text
                                     , _code       :: Maybe Text
                                     , _inPorts    :: Map InPort Port
                                     , _outPorts   :: OutPortTree Port
                                     , _nodeMeta   :: NodeMeta
                                     , _canEnter   :: Bool
                                     } deriving (Generic, Eq, NFData, Show, Typeable)

data InputSidebar = InputSidebar { _inputNodeId    :: NodeId
                                 , _inputEdgePorts :: [OutPortTree Port]
                                 } deriving (Generic, Eq, NFData, Show, Typeable)

data OutputSidebar = OutputSidebar { _outputNodeId    :: NodeId
                                   , _outputEdgePorts :: Map InPort Port
                                   } deriving (Generic, Eq, NFData, Show, Typeable)

data NodeTypecheckerUpdate = NodeTypecheckerUpdate {
      _tcNodeId   :: NodeId
    , _tcInPorts  :: Map InPort Port
    , _tcOutPorts :: OutPortTree Port
    } deriving (Generic, Eq, NFData, Show, Typeable)

makeLenses ''ExpressionNode
makeLenses ''InputSidebar
makeLenses ''OutputSidebar
makeLenses ''NodeTypecheckerUpdate

position :: Lens' ExpressionNode (Double, Double)
position = nodeMeta . NodeMeta.position

instance Binary ExpressionNode
instance Binary InputSidebar
instance Binary OutputSidebar
instance Binary NodeTypecheckerUpdate

makePortsMap :: [Port] -> Map PortId Port
makePortsMap = Map.fromList . map (view Port.portId &&& id)

class HasNodeId a where
    nodeId :: Lens' a NodeId

instance HasNodeId ExpressionNode where
    nodeId = exprNodeId

instance HasNodeId InputSidebar where
    nodeId = inputNodeId

instance HasNodeId OutputSidebar where
    nodeId = outputNodeId
