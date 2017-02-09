{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Port where

import           Data.Aeson              (FromJSON, ToJSON)
import           Empire.API.Data.PortRef (AnyPortRef)
import           Luna.Studio.Prelude
import           React.Flux              (MouseEvent)



data Event = MouseDown       MouseEvent AnyPortRef
           | MouseUp         AnyPortRef
           | Click           MouseEvent AnyPortRef
           | MouseEnter      AnyPortRef
           | MouseLeave      AnyPortRef
           deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
