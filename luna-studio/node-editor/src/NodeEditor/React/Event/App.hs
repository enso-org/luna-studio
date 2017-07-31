{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.App where

import           Common.Prelude
import           Data.ScreenPosition (ScreenPosition)
import           Data.Timestamp      (Timestamp)
import           React.Flux          (KeyboardEvent, MouseEvent)



data Event = MouseDown  MouseEvent Timestamp
           | Movement   ScreenPosition
           | MouseMove  MouseEvent Timestamp
           | MouseUp    MouseEvent
           | Click
           | KeyDown    KeyboardEvent
           | MouseLeave
           | Resize
            deriving (Show, Generic, NFData, Typeable)
