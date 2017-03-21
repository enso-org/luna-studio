module Empire.API.Data.MonadPath where

import           Data.Binary             (Binary)
import           Empire.API.Data.Node    (Node, NodeId)
import           Prologue                hiding (TypeRep)

import           Empire.API.Data.TypeRep (TypeRep)



data MonadPath = MonadPath
    { _monadType :: TypeRep
    , _path      :: [NodeId]
    } deriving (Generic, Eq, NFData, Show, Typeable)

makeLenses ''MonadPath

instance Binary MonadPath
