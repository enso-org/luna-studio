{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Atom where

import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON, ToJSON)
import           Luna.Studio.Prelude


data AtomEvent = OpenSearcher
               | Accept
               | Cancel
               | SelectAll
               deriving (Read, Show, Generic, NFData, Typeable)

instance ToJSON   AtomEvent
instance FromJSON AtomEvent
