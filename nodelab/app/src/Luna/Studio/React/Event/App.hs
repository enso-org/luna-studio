{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.App where

import           Data.Aeson          (FromJSON, ToJSON)
import           React.Flux          (KeyboardEvent, MouseEvent)

import           Luna.Studio.Prelude



data Event = MouseDown  MouseEvent
           | MouseMove  MouseEvent
           | MouseUp    MouseEvent
           | Click      MouseEvent
           | KeyDown    KeyboardEvent
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
