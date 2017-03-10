{-# LANGUAGE Rank2Types #-}
module Empire.API.Data.Connection where

import           Data.Binary             (Binary)
import           Prologue

import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.Port    (InPort, OutPort)
import           Empire.API.Data.PortRef (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef as PortRef


type ConnectionId = InPortRef
data Connection = Connection { _src :: OutPortRef
                             , _dst :: InPortRef
                             } deriving (Generic, Eq, NFData, Show)

makeLenses ''Connection
instance Binary Connection

connectionId :: Lens' Connection ConnectionId
connectionId = dst

srcNodeId :: Lens' Connection NodeId
srcNodeId = src . PortRef.srcNodeId

srcPortId :: Lens' Connection OutPort
srcPortId = src . PortRef.srcPortId

dstNodeId :: Lens' Connection NodeId
dstNodeId = dst . PortRef.dstNodeId

dstPortId :: Lens' Connection InPort
dstPortId = dst . PortRef.dstPortId

raw :: Getter Connection (OutPortRef, InPortRef)
raw = to raw' where
    raw' conn = (conn ^. src, conn ^. dst)

nodeIds :: Getter Connection (NodeId, NodeId)
nodeIds = to nodeIds' where
    nodeIds' conn = ( conn ^. src . PortRef.srcNodeId
                    , conn ^. dst . PortRef.dstNodeId )

containsNode :: NodeId -> Connection -> Bool
containsNode nid conn = (conn ^. srcNodeId == nid)
                     || (conn ^. dstNodeId == nid)

containsPortRef :: AnyPortRef -> Connection -> Bool
containsPortRef (InPortRef'  inPortRef)  conn = conn ^. dst == inPortRef
containsPortRef (OutPortRef' outPortRef) conn = conn ^. src == outPortRef

toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe Connection
toValidConnection (OutPortRef' src') (InPortRef' dst') =
    if src' ^. PortRef.srcNodeId /= dst' ^. PortRef.dstNodeId then
        Just $ Connection src' dst'
    else Nothing
toValidConnection dst'@(InPortRef' _) src'@(OutPortRef' _) = toValidConnection src' dst'
toValidConnection _ _ = Nothing
