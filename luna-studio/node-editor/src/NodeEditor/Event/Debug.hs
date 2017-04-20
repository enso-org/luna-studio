{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Event.Debug where

import           Data.Aeson          (ToJSON)
import           Common.Prelude


data Event = GetState deriving (Eq, Show, Generic, NFData)

makeLenses ''Event

instance ToJSON Event
