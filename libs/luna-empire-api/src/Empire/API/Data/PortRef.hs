module Empire.API.Data.PortRef where

import           Control.DeepSeq      (NFData)
import           Data.Binary          (Binary)
import           Empire.API.Data.Node (NodeId)
import           Empire.API.Data.Port (InPort, OutPort, PortId (..))
import           Prologue

data InPortRef  = InPortRef  { _dstNodeId :: NodeId
                             , _dstPortId :: InPort
                             } deriving (Show, Eq, Generic, Ord, NFData)

data OutPortRef = OutPortRef { _srcNodeId :: NodeId
                             , _srcPortId :: OutPort
                             } deriving (Show, Eq, Generic, Ord, NFData)

data AnyPortRef = OutPortRef' OutPortRef | InPortRef' InPortRef deriving (Show, Eq, Generic, NFData)

instance Ord AnyPortRef where
  (InPortRef'  _)  `compare` (OutPortRef' _) = LT
  (OutPortRef' _)  `compare` (InPortRef'  _) = GT
  (InPortRef'  a)  `compare` (InPortRef'  b) = a `compare` b
  (OutPortRef' a)  `compare` (OutPortRef' b) = a `compare` b

nodeId' :: AnyPortRef -> NodeId
nodeId' (OutPortRef' (OutPortRef nid _)) = nid
nodeId' (InPortRef'  (InPortRef  nid _)) = nid

nodeId :: Getter AnyPortRef NodeId
nodeId = to nodeId'

portId' :: AnyPortRef -> PortId
portId' (OutPortRef' (OutPortRef _ pid)) = OutPortId pid
portId' (InPortRef'  (InPortRef  _ pid)) = InPortId  pid

portId :: Getter AnyPortRef PortId
portId = to portId'

toAnyPortRef :: NodeId -> PortId -> AnyPortRef
toAnyPortRef nid (InPortId pid)  = InPortRef'  $ InPortRef  nid pid
toAnyPortRef nid (OutPortId pid) = OutPortRef' $ OutPortRef nid pid

makeLenses ''AnyPortRef
makePrisms ''AnyPortRef
makeLenses ''OutPortRef
makePrisms ''OutPortRef
makeLenses ''InPortRef
makePrisms ''InPortRef

instance Binary AnyPortRef
instance Binary InPortRef
instance Binary OutPortRef
