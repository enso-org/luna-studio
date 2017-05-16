{-# LANGUAGE DeriveAnyClass #-}

module NodeEditor.React.Event.Port where

import           Data.Aeson              (FromJSON, ToJSON)
import           LunaStudio.Data.PortRef (AnyPortRef)
import           Common.Prelude
import           React.Flux              (MouseEvent)



data Event = Click         MouseEvent AnyPortRef
           | MouseDown     MouseEvent AnyPortRef
           | MouseEnter    AnyPortRef
           | MouseLeave    AnyPortRef
           | MouseUp       AnyPortRef
           deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
