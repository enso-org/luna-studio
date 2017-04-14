{-# LANGUAGE TypeFamilies #-}
module Data.Timestamp where

import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON, ToJSON)
import           Luna.Studio.Prelude


newtype Timestamp = Timestamp Int deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON Timestamp
instance FromJSON Timestamp
makeWrapped ''Timestamp
