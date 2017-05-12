module LunaStudio.Data.MonadPath where

import           Data.Binary             (Binary)
import           LunaStudio.Data.Node    (NodeId)
import           Prologue                hiding (TypeRep)
import           LunaStudio.Data.TypeRep (TypeRep)


data MonadPath = MonadPath { _monadType :: TypeRep
                           , _path      :: [NodeId]
                           } deriving (Eq, Generic, NFData, Show, Typeable)

makeLenses ''MonadPath
instance Binary MonadPath
