{-# LANGUAGE Rank2Types #-}
module Empire.API.Data.Connection where

import           Data.Binary             (Binary)
import           Prologue

import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.PortRef (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef, dstNodeId, srcNodeId)
import qualified Empire.API.Data.PortRef as PortRef


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
contains' nid (Connection src' dst') = (src' ^. srcNodeId == nid)
                                    || (dst' ^. dstNodeId == nid)

contains :: NodeId -> Getter Connection Bool
contains nid = to (contains' nid)

toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe Connection
toValidConnection (OutPortRef' src') (InPortRef' dst') =
    if src' ^. PortRef.srcNodeId /= dst' ^. PortRef.dstNodeId then
        Just $ Connection src' dst'
    else Nothing
toValidConnection dst'@(InPortRef' _) src'@(OutPortRef' _) = toValidConnection src' dst'
toValidConnection _ _ = Nothing
