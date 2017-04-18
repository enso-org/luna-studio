{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.App where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Timestamp      (Timestamp)
import           React.Flux          (KeyboardEvent, MouseEvent)

import           Luna.Prelude



data Event = MouseDown  MouseEvent Timestamp
           | MouseMove  MouseEvent Timestamp
           | MouseUp    MouseEvent
           | Click
           | KeyDown    KeyboardEvent
           | MouseLeave
           | Resize
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
