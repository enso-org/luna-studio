module Empire.Data.FileMetadata where

import Empire.Prelude

import Data.Aeson               (FromJSON, ToJSON)
import LunaStudio.Data.NodeMeta (NodeMeta)


data MarkerNodeMeta = MarkerNodeMeta
    { marker :: Word64
    , meta   :: NodeMeta
    } deriving (Eq, Generic, Show)

instance FromJSON MarkerNodeMeta
instance ToJSON   MarkerNodeMeta

toTuple :: MarkerNodeMeta -> (Word64, NodeMeta)
toTuple (MarkerNodeMeta marker' meta') = (marker', meta')

newtype FileMetadata = FileMetadata
    { metas :: [MarkerNodeMeta]
    } deriving (Generic, Show)

instance FromJSON FileMetadata
instance ToJSON   FileMetadata

toList :: FileMetadata -> [MarkerNodeMeta]
toList (FileMetadata metas') = metas'
