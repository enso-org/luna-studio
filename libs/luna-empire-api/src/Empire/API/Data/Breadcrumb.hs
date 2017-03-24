{-# LANGUAGE TypeFamilies #-}
module Empire.API.Data.Breadcrumb where

import           Control.DeepSeq      (NFData)
import           Data.Binary          (Binary)
import           Data.Monoid          (Monoid (..))
import           Empire.API.Data.Node (NodeId)
import           Prologue             hiding (Monoid, mappend, mempty)


data BreadcrumbItem = Lambda NodeId
                    | Arg NodeId Int
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

instance Default (Breadcrumb a) where
    def = mempty
