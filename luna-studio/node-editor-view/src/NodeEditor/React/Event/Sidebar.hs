{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.Sidebar where

import           Common.Data.Event      (EventName)
import           Common.Prelude
import           Data.Timestamp          (Timestamp)
import           LunaStudio.Data.NodeLoc (NodeLoc)
import           LunaStudio.Data.PortRef (AnyPortRef, OutPortRef)
import           React.Flux              (MouseEvent)


data Event = AddPort           AnyPortRef
           | MouseMove         MouseEvent NodeLoc Timestamp
           | RemovePort        AnyPortRef
           | EditPortName      OutPortRef
           | ToggleInputMode   NodeLoc
           | ToggleOutputMode  NodeLoc
           | UnfreezeSidebar   NodeLoc
            deriving (Show, Generic, NFData, Typeable)

instance EventName Event
