{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module LunaStudio.Data.Breadcrumb where

import           Control.DeepSeq      (NFData)
import           Control.Lens.Aeson   (lensJSONParse, lensJSONToEncoding, lensJSONToJSON)
import           Data.Aeson.Types     (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import           Data.Binary          (Binary)
import           Data.Monoid          (Monoid (..))
import           Data.Semigroup       (Semigroup (..))
import qualified Data.Text            as Text
import           LunaStudio.Data.Node (NodeId)
import           Prologue             hiding (Monoid, mappend, mconcat, mempty, (<>))


data BreadcrumbItem = Definition { _nodeId  :: NodeId }
                    | Lambda     { _nodeId  :: NodeId }
                    | Arg        { _nodeId  :: NodeId, _arg :: Int }
                    deriving (Eq, Generic, NFData, Ord, Show)

data Named a = Named { _name       :: Text
                     , _breadcrumb :: a
                     } deriving (Eq, Generic, NFData, Show)

newtype Breadcrumb a = Breadcrumb { _items :: [a] } deriving (Eq, Generic, NFData, Ord, Show)

makeLenses ''BreadcrumbItem
makeLenses ''Breadcrumb
makeLenses ''Named
instance Binary a => Binary (Breadcrumb a)
instance Binary a => Binary (Named a)
instance Binary BreadcrumbItem

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
toNames = Breadcrumb . map (view name) . view items

instance FromJSON a => FromJSONKey (Breadcrumb a)
instance {-# OVERLAPPABLE #-} FromJSON a => FromJSON (Breadcrumb a) where parseJSON = lensJSONParse
instance FromJSON a => FromJSON (Named a)      where parseJSON = lensJSONParse
instance ToJSON a => ToJSONKey (Breadcrumb a)
instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (Breadcrumb a) where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding

instance ToJSON (Breadcrumb Text) where
    toJSON     = toJSON . intercalate "." . unwrap

instance FromJSON (Breadcrumb Text) where parseJSON = fmap (Breadcrumb . Text.split (== '.')) . parseJSON

instance ToJSON a => ToJSON (Named a) where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding

instance FromJSON BreadcrumbItem where parseJSON = lensJSONParse
instance FromJSONKey BreadcrumbItem
instance ToJSON BreadcrumbItem where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding
instance ToJSONKey BreadcrumbItem
