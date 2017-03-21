{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Port where

import           Data.Aeson              (FromJSON, ToJSON)
import           Empire.API.Data.PortRef (AnyPortRef)
import           Luna.Studio.Prelude
import           React.Flux              (MouseEvent)



data Event = Click         MouseEvent AnyPortRef
           | MouseDown     MouseEvent AnyPortRef
           | MouseEnter    AnyPortRef
           | MouseLeave    AnyPortRef
           | MouseUp       AnyPortRef
           deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
