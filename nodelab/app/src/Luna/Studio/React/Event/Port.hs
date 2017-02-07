{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Port where

import           Data.Aeson              (FromJSON, ToJSON)
import           Empire.API.Data.PortRef (AnyPortRef)
import           Luna.Studio.Prelude



data Event = MouseEnter      AnyPortRef
           | MouseLeave      AnyPortRef
           deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
