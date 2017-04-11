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
import           Empire.API.Data.Port     (AnyPortId, InPort, InPortId, InPortTree, OutPort, OutPortTree)
import qualified Empire.API.Data.Port     as Port
import           Prologue


type NodeId = UUID

data ExpressionNode = ExpressionNode { _exprNodeId :: NodeId
                                     , _expression :: Text
                                     , _name       :: Maybe Text
                                     , _code       :: Maybe Text
                                     , _inPorts    :: InPortTree  InPort
                                     , _outPorts   :: OutPortTree OutPort
                                     , _nodeMeta   :: NodeMeta
                                     , _canEnter   :: Bool
                                     } deriving (Generic, Eq, NFData, Show, Typeable)

data InputSidebar = InputSidebar { _inputNodeId    :: NodeId
                                 , _inputEdgePorts :: [OutPortTree OutPort]
                                 } deriving (Generic, Eq, NFData, Show, Typeable)

data OutputSidebar = OutputSidebar { _outputNodeId    :: NodeId
                                   , _outputEdgePorts :: InPortTree InPort
                                   } deriving (Generic, Eq, NFData, Show, Typeable)

data NodeTypecheckerUpdate = ExpressionUpdate    { _tcNodeId   :: NodeId, _tcInPorts       :: InPortTree InPort, _tcOutPorts :: OutPortTree OutPort }
                           | OutputSidebarUpdate { _tcNodeId   :: NodeId, _tcInPorts       :: InPortTree InPort }
                           | InputSidebarUpdate  { _tcNodeId   :: NodeId, _tcInputOutPorts :: [OutPortTree OutPort] }
                           deriving (Generic, Eq, NFData, Show, Typeable)

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

class HasNodeId a where
    nodeId :: Lens' a NodeId

instance HasNodeId ExpressionNode where
    nodeId = exprNodeId

instance HasNodeId InputSidebar where
    nodeId = inputNodeId

instance HasNodeId OutputSidebar where
    nodeId = outputNodeId
