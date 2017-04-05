module Empire.API.Data.Graph where

import           Data.Binary               (Binary)
import           Prologue                  hiding (TypeRep)

import           Empire.API.Data.MonadPath (MonadPath)
import           Empire.API.Data.Node      (ExpressionNode, InputSidebar, OutputSidebar, NodeId)
import           Empire.API.Data.PortRef   (InPortRef, OutPortRef)
import           Empire.API.Data.TypeRep   (TypeRep)


data Graph = Graph { _nodes         :: [ExpressionNode]
                   , _connections   :: [(OutPortRef, InPortRef)]
                   , _inputSidebar  :: InputSidebar
                   , _outputSidebar :: OutputSidebar
                   , _monads        :: [MonadPath]
                   } deriving (Generic, Eq, NFData, Show)

makeLenses ''Graph
instance Binary Graph
