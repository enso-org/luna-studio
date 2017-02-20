{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Internal where

import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.List           as List
import           Luna.Studio.Prelude


data DataType = TextDiff
             deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data InternalEvent = InternalEvent
                   { _dataType  :: DataType
                   , _text      :: Text
                   , _start     :: Int
                   , _end       :: Int
                   , _cursor    :: [Int]
                   } deriving (Generic, NFData, Show, Typeable)

makeLenses ''InternalEvent

instance ToJSON   DataType
instance FromJSON DataType
instance ToJSON   InternalEvent
instance FromJSON InternalEvent
