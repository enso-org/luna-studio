{-# LANGUAGE DeriveAnyClass #-}
module Internal.Event.Debug where

import           Data.Aeson          (ToJSON)
import           Internal.Prelude


data Event = GetState deriving (Eq, Show, Generic, NFData)

makeLenses ''Event

instance ToJSON Event
