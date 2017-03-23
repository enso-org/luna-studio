{-# LANGUAGE TypeFamilies #-}
module Empire.API.Data.Breadcrumb where

import           Control.DeepSeq      (NFData)
import           Data.Binary          (Binary)
import           Empire.API.Data.Node (NodeId)
import           Prologue


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

instance Default (Breadcrumb a) where
    def = Breadcrumb def
