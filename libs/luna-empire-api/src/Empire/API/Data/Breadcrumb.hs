module Empire.API.Data.Breadcrumb where

import           Data.Binary          (Binary)
import           Empire.API.Data.Node (NodeId)
import           Prologue

data BreadcrumbItem = Lambda NodeId   deriving (Show, Eq, Generic)
data Named a        = Named  { _name       :: Text
                             , _breadcrumb :: a
                             } deriving (Show, Eq, Generic)

newtype Breadcrumb a = Breadcrumb { _items :: [a] } deriving (Show, Eq, Generic)

makeLenses ''Breadcrumb
makeLenses ''Named

instance Binary a => Binary (Breadcrumb a)
instance Binary a => Binary (Named a)
instance Binary BreadcrumbItem
