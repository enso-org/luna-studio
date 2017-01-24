{-# LANGUAGE DeriveAnyClass #-}
module Empire.API.Data.Breadcrumb where

import           Control.DeepSeq      (NFData)
import           Data.Binary          (Binary)
import           Data.Text            (Text)
import           Prologue             hiding (Text)

import           Empire.API.Data.Node (NodeId)
import           Data.Text.Lazy (Text)
import           Prologue hiding (Text)



data BreadcrumbItem = Lambda NodeId   deriving (Show, Eq, Generic, NFData)
data Named a        = Named  { _name       :: Text
                             , _breadcrumb :: a
                             } deriving (Show, Eq, Generic)

newtype Breadcrumb a = Breadcrumb { _items :: [a] } deriving (Show, Eq, Generic, NFData)

makeLenses ''Breadcrumb
makeLenses ''Named

instance Binary a => Binary (Breadcrumb a)
instance Binary a => Binary (Named a)
instance Binary BreadcrumbItem

instance Default (Breadcrumb a) where
    def = Breadcrumb def
