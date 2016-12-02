{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Event.App where

import           Control.DeepSeq   (NFData)
import           Data.Aeson        (FromJSON, ToJSON)

import           Utils.PreludePlus



data Event = ToggleCodeEditor
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
