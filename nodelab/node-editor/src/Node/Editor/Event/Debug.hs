{-# LANGUAGE DeriveAnyClass #-}
module Node.Editor.Event.Debug where

import           Data.Aeson          (ToJSON)
import           Luna.Prelude


data Event = GetState deriving (Eq, Show, Generic, NFData)

makeLenses ''Event

instance ToJSON Event
