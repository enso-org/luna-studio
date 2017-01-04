{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.NodeEditor where

import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON, ToJSON)
import           React.Flux          (MouseEvent, WheelEvent)
import qualified React.Flux          as RF

import           Luna.Studio.Prelude



data Event = MouseDown  MouseEvent
           | Wheel      MouseEvent WheelEvent
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
