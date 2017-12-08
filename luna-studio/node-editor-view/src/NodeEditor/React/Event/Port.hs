{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}

module NodeEditor.React.Event.Port where

import           Common.Data.Event       (EventName)
import           Common.Prelude
import           LunaStudio.Data.PortRef (AnyPortRef)
import           React.Flux              (MouseEvent)


data Event = Click         MouseEvent AnyPortRef
           | MouseDown     MouseEvent AnyPortRef
           | MouseEnter    AnyPortRef
           | MouseLeave    AnyPortRef
           | MouseUp       AnyPortRef
           deriving (Show, Generic, NFData, Typeable)

instance EventName Event
