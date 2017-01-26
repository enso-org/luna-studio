{-# LANGUAGE Rank2Types #-}
module Empire.API.Data.Connection where

import           Data.Binary             (Binary)
import           Prologue

import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.PortRef (InPortRef, OutPortRef, dstNodeId, srcNodeId)


-- FIXME: Najpewniej to wyladuje calkowicie w GUI

type ConnectionId = InPortRef
data Connection = Connection { _src :: OutPortRef
                             , _dst :: InPortRef
                             } deriving (Generic, Eq, NFData, Show)

makeLenses ''Connection
instance Binary Connection

connectionId :: Lens' Connection ConnectionId
connectionId = dst

contains' :: NodeId -> Connection -> Bool
contains' nid (Connection src dst) = (src ^. srcNodeId == nid)
                                  || (dst ^. dstNodeId == nid)

contains :: NodeId -> Getter Connection Bool
contains nid = to (contains' nid)
