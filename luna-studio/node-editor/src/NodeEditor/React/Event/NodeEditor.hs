{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.NodeEditor where

import           Data.Aeson          (FromJSON, ToJSON)
import           React.Flux          (MouseEvent, WheelEvent)

import           Common.Prelude



data Event = ContextMenu
           | MouseDown  MouseEvent
           | Wheel      MouseEvent WheelEvent
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
