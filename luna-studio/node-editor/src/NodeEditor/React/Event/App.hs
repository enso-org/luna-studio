{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.App where

import           Data.Timestamp (Timestamp)
import           React.Flux     (KeyboardEvent, MouseEvent)

import           Common.Prelude



data Event = MouseDown  MouseEvent Timestamp
           | MouseMove  MouseEvent Timestamp
           | MouseUp    MouseEvent
           | Click
           | KeyDown    KeyboardEvent
           | MouseLeave
           | Resize
            deriving (Show, Generic, NFData, Typeable)
