module LunaStudio.Data.NodeCache where

import           Data.Aeson.Types         (FromJSON, ToJSON)
import           Data.Binary              (Binary)
import           Data.Map                 (Map)
import           LunaStudio.Data.NodeId   (NodeId)
import           LunaStudio.Data.NodeMeta (NodeMeta)
import           Prologue


data NodeCache = NodeCache
    { _nodeIdMap      :: Map Word64 NodeId
    , _nodeMetaMap    :: Map Word64 NodeMeta
    , _portMappingMap :: Map (NodeId, Maybe Int) (NodeId, NodeId)
    } deriving (Generic, Eq, Show)

makeLenses ''NodeCache

instance Binary   NodeCache
instance NFData   NodeCache
instance FromJSON NodeCache
instance ToJSON   NodeCache

instance Default NodeCache where
    def = NodeCache def def def
