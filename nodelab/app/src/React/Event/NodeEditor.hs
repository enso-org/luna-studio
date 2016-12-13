{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Event.NodeEditor where

import           Control.DeepSeq   (NFData)
import           Data.Aeson        (FromJSON, ToJSON)
import           React.Flux        (MouseEvent)

import           Luna.Studio.Prelude



data Event = MouseDown  MouseEvent
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
