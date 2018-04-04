module LunaStudio.Data.MonadPath where

import           Data.Aeson.Types        (FromJSON, ToJSON)
import           Data.Binary             (Binary)
import           LunaStudio.Data.NodeId  (NodeId)
import           LunaStudio.Data.TypeRep (TypeRep)
import           Prologue                hiding (TypeRep)


data MonadPath = MonadPath
    { _monadType :: TypeRep
    , _path      :: [NodeId]
    } deriving (Eq, Generic, Show, Typeable)

makeLenses ''MonadPath

instance Binary   MonadPath
instance NFData   MonadPath
instance FromJSON MonadPath
instance ToJSON   MonadPath
