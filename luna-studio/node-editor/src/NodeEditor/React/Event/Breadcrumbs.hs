{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.Breadcrumbs where

import           Data.Aeson                 (FromJSON, ToJSON)

import           Empire.API.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)
import           Common.Prelude



data Event = Enter (Breadcrumb BreadcrumbItem)
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
