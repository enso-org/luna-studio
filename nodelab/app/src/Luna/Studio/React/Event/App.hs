{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.App where

import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON, ToJSON)
import           React.Flux          (KeyboardEvent, MouseEvent)
import qualified React.Flux          as React (Event)

import           Luna.Studio.Prelude



data Event = MouseDown  MouseEvent
           | MouseMove  MouseEvent
           | MouseUp    MouseEvent
           | Click      MouseEvent
           | KeyDown    KeyboardEvent
           | Copy       React.Event
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
