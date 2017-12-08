{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.App where

import           Common.Data.Event              (EventName)
import           Common.Prelude
import           Data.Timestamp                 (Timestamp)
import           LunaStudio.Data.ScreenPosition (ScreenPosition)
import           React.Flux                     (KeyboardEvent, MouseEvent, WheelEvent)


data Event = Click
           | ContextMenu
           | KeyDown       KeyboardEvent
           | Movement      ScreenPosition
           | MouseDown     MouseEvent Timestamp
           | MouseLeave
           | MouseMove     MouseEvent Timestamp
           | MouseUp       MouseEvent
           | Resize
           | Wheel         MouseEvent WheelEvent
           deriving (Show, Generic, NFData, Typeable)

instance EventName Event
