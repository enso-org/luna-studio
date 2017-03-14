module Empire.API.Data.Graph where

import           Data.Binary               (Binary)
import           Prologue                  hiding (TypeRep)

import           Empire.API.Data.MonadPath (MonadPath)
import           Empire.API.Data.Node      (Node, NodeId)
import           Empire.API.Data.PortRef   (InPortRef, OutPortRef)
import           Empire.API.Data.TypeRep   (TypeRep)


data Graph = Graph { _nodes       :: [Node]
                   , _connections :: [(OutPortRef, InPortRef)]
                   , _monads      :: [MonadPath]
                   } deriving (Generic, Eq, NFData, Show)

makeLenses ''Graph
instance Binary Graph
