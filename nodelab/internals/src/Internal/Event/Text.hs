{-# LANGUAGE DeriveAnyClass #-}
module Internal.Event.Text where

import           Data.Aeson          (FromJSON, ToJSON)
import           Internal.Prelude



data TextEvent = TextEvent
               { _filePath  :: FilePath
               , _start     :: Int
               , _stop      :: Int
               , _text      :: Text
               , _cursor    :: Maybe Int
               } deriving (Generic, NFData, Show, Typeable)

makeLenses ''TextEvent

instance ToJSON   TextEvent
instance FromJSON TextEvent
