{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Event.Searcher where

import           Control.DeepSeq   (NFData)
import           Data.Aeson        (FromJSON, ToJSON)
import           React.Flux        (KeyboardEvent)

import           Utils.PreludePlus



data Event = InputChanged Text
           | KeyDown KeyboardEvent
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
