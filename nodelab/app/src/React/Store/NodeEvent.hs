{-# LANGUAGE DeriveAnyClass #-}

module React.Store.NodeEvent where

import           Control.DeepSeq   (NFData)
import           Data.Aeson        (FromJSON, ToJSON)

import           Utils.PreludePlus



data Event = OnClick
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
