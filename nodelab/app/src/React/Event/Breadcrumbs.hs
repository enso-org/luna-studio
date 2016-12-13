{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Event.Breadcrumbs where

import           Control.DeepSeq            (NFData)
import           Data.Aeson                 (FromJSON, ToJSON)

import           Empire.API.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)
import           Empire.API.JSONInstances   ()
import           Luna.Studio.Prelude



data Event = Enter (Breadcrumb BreadcrumbItem)
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
