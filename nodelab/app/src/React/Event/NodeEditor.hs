{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Event.NodeEditor where

import           Control.DeepSeq   (NFData)
import           Data.Aeson        (FromJSON, ToJSON)
import           React.Flux        (MouseEvent)

import           Utils.PreludePlus



data Event = MouseDown  MouseEvent
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
