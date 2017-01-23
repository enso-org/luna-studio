{-# LANGUAGE DeriveAnyClass #-}
module Event.Atom where

import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON, ToJSON)
import           Luna.Studio.Prelude


data AtomEvent = Event
               deriving (Show, Generic, NFData, Typeable)

instance ToJSON   AtomEvent
instance FromJSON AtomEvent
