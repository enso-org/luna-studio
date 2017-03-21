{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.Breadcrumbs where

import           Data.Aeson                 (FromJSON, ToJSON)

import           Empire.API.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)
import           Luna.Studio.Prelude



data Event = Enter (Breadcrumb BreadcrumbItem)
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
