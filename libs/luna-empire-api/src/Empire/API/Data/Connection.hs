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
