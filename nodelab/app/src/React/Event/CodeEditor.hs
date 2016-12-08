{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Event.CodeEditor where

import           Control.DeepSeq   (NFData)
import           Data.Aeson        (FromJSON, ToJSON)

import           Utils.PreludePlus



data Event = ToggleCodeEditor
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
