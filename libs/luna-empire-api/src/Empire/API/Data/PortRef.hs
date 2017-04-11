module Empire.API.Data.PortRef
( module Empire.API.Data.PortRef
, nodeLoc
) where

import           Control.DeepSeq         (NFData)
import           Data.Binary             (Binary)
import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.NodeLoc (HasNodeLoc (..), NodeLoc)
import qualified Empire.API.Data.NodeLoc as NodeLoc
import           Empire.API.Data.Port    (InPort, OutPort, PortId (..))
import           Prologue

data InPortRef  = InPortRef  { _dstNodeLoc :: NodeLoc
                             , _dstPortId :: InPort
                             } deriving (Show, Eq, Generic, Ord, NFData)

data OutPortRef = OutPortRef { _srcNodeLoc :: NodeLoc
                             , _srcPortId :: OutPort
                             } deriving (Show, Eq, Generic, Ord, NFData)

data AnyPortRef = OutPortRef' OutPortRef | InPortRef' InPortRef deriving (Show, Eq, Generic, NFData)

makeLenses ''AnyPortRef
makePrisms ''AnyPortRef
makeLenses ''OutPortRef
makePrisms ''OutPortRef
makeLenses ''InPortRef
makePrisms ''InPortRef

instance Binary AnyPortRef
instance Binary InPortRef
instance Binary OutPortRef

instance Ord AnyPortRef where
  (InPortRef'  _)  `compare` (OutPortRef' _) = LT
  (OutPortRef' _)  `compare` (InPortRef'  _) = GT
  (InPortRef'  a)  `compare` (InPortRef'  b) = a `compare` b
  (OutPortRef' a)  `compare` (OutPortRef' b) = a `compare` b

instance HasNodeLoc InPortRef  where nodeLoc = dstNodeLoc
instance HasNodeLoc OutPortRef where nodeLoc = srcNodeLoc
instance HasNodeLoc AnyPortRef where
    nodeLoc = lens getNodeLoc setNodeLoc  where
        getNodeLoc (OutPortRef' outPortRef) = outPortRef ^. nodeLoc
        getNodeLoc (InPortRef'  inPortRef)  = inPortRef  ^. nodeLoc
        setNodeLoc (OutPortRef' outPortRef) nl = OutPortRef' $ outPortRef & nodeLoc .~ nl
        setNodeLoc (InPortRef'  inPortRef ) nl = InPortRef'  $ inPortRef  & nodeLoc .~ nl

{-# DEPRECATED nodeId' "Use nodeLoc'" #-}
nodeId' :: AnyPortRef -> NodeId
nodeId' (OutPortRef' (OutPortRef nl _)) = nl ^. NodeLoc.nodeId
nodeId' (InPortRef'  (InPortRef  nl _)) = nl ^. NodeLoc.nodeId

{-# DEPRECATED nodeId "Use nodeLoc" #-}
nodeId :: Getter AnyPortRef NodeId
nodeId = to nodeId'

portId' :: AnyPortRef -> PortId
portId' (OutPortRef' (OutPortRef _ pid)) = OutPortId pid
portId' (InPortRef'  (InPortRef  _ pid)) = InPortId  pid

portId :: Getter AnyPortRef PortId
portId = to portId'

dstNodeId :: Lens' InPortRef NodeId
dstNodeId = dstNodeLoc . NodeLoc.nodeId

srcNodeId :: Lens' OutPortRef NodeId
srcNodeId = srcNodeLoc . NodeLoc.nodeId

toAnyPortRef :: NodeLoc -> PortId -> AnyPortRef
toAnyPortRef nl (InPortId pid)  = InPortRef'  $ InPortRef  nl pid
toAnyPortRef nl (OutPortId pid) = OutPortRef' $ OutPortRef nl pid
