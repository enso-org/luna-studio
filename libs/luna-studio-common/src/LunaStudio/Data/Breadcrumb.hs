module LunaStudio.Data.Breadcrumb where

import           Control.DeepSeq        (NFData)
import           Control.Lens           (toListOf, traversed)
import qualified Control.Lens.Aeson     as Lens
import           Data.Aeson.Types       (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import           Data.Binary            (Binary)
import           Data.Monoid            (Monoid (..))
import           Data.Semigroup         (Semigroup (..))
import qualified Data.List              as List
import qualified Data.Text              as Text
import           LunaStudio.Data.NodeId (NodeId)
import           Prologue               hiding (Monoid, mappend, mconcat, mempty, (<>))


data BreadcrumbItem
    = Definition { _nodeId  :: NodeId }
    | Lambda     { _nodeId  :: NodeId }
    | Arg        { _nodeId  :: NodeId, _arg :: Int }
    deriving (Eq, Generic, Ord, Read, Show)

data Named a = Named
    { _name       :: Text
    , _breadcrumb :: a
    } deriving (Eq, Generic, Show)

newtype Breadcrumb a = Breadcrumb
    { _items :: [a]
    } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''BreadcrumbItem
makeLenses ''Breadcrumb
makeLenses ''Named

instance Binary a => Binary (Breadcrumb a)
instance Binary a => Binary (Named a)
instance NFData a => NFData (Breadcrumb a)
instance NFData a => NFData (Named a)
instance Binary BreadcrumbItem
instance NFData BreadcrumbItem

instance Monoid (Breadcrumb a) where
    mappend bc1 bc2 = Breadcrumb $ (bc1 ^. items) <> (bc2 ^. items)
    mempty = Breadcrumb def

instance Semigroup (Breadcrumb a) where
    (<>) = mappend

instance Default (Breadcrumb a) where
    def = mempty

containsNode :: Breadcrumb BreadcrumbItem -> NodeId -> Bool
containsNode b nid = any ((nid ==) . view nodeId) $ b ^. items

toNames :: Breadcrumb (Named BreadcrumbItem) -> Breadcrumb Text
toNames = Breadcrumb . toListOf (items . traversed . name)

instance FromJSON a => FromJSONKey (Breadcrumb a)
instance ToJSON   a => ToJSONKey   (Breadcrumb a)
instance FromJSONKey BreadcrumbItem
instance ToJSONKey  BreadcrumbItem

instance {-# OVERLAPPABLE #-} FromJSON a => FromJSON (Breadcrumb a) where parseJSON = Lens.parse
instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (Breadcrumb a) where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance FromJSON a => FromJSON (Named a) where parseJSON = Lens.parse
instance ToJSON   a => ToJSON   (Named a) where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance FromJSON (Breadcrumb Text) where
    parseJSON = fmap (Breadcrumb . Text.split (== '.')) . parseJSON
instance ToJSON (Breadcrumb Text) where
    toJSON = toJSON . intercalate "." . unwrap

instance FromJSON BreadcrumbItem where parseJSON = Lens.parse
instance ToJSON   BreadcrumbItem where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

namedInits :: Breadcrumb (Named a) -> [Named (Breadcrumb a)]
namedInits bc = zipWith Named names (Breadcrumb <$> List.inits unnamed) where
    items'  = bc ^. items
    names   = "" : (_name <$> items')
    unnamed = _breadcrumb <$> items'
