{-# LANGUAGE TypeFamilies #-}
module Empire.API.Data.Breadcrumb where

import           Control.DeepSeq      (NFData)
import           Data.Binary          (Binary)
import           Data.Monoid          (Monoid (..))
import           Data.Semigroup       (Semigroup (..))
import           Empire.API.Data.Node (NodeId)
import           Prologue             hiding (Monoid, mappend, mempty, (<>))


data BreadcrumbItem = Lambda { _nodeId :: NodeId }
                    | Arg    { _nodeId :: NodeId, _arg :: Int }
                    deriving (Show, Eq, Ord, Generic, NFData)

data Named a        = Named  { _name       :: Text
                             , _breadcrumb :: a
                             } deriving (Generic, Eq, NFData, Show)

newtype Breadcrumb a = Breadcrumb { _items :: [a] } deriving (Show, Eq, Ord, Generic, NFData)

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
