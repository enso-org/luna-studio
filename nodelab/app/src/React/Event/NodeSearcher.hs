{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Event.NodeSearcher where

import           Control.DeepSeq   (NFData)
import           Data.Aeson        (FromJSON, ToJSON)

import           Utils.PreludePlus



data Event = Display
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
