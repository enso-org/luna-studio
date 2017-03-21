{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Internal where

import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.List           as List
import           Luna.Studio.Prelude


data ActionType = CloseFile
                  | OpenFile
                  | SaveFile
                  | SetProject
                  | GetBuffer
                  deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data InternalEvent = InternalEvent
                   { _dataType  :: ActionType
                   , _path      :: String
                   } deriving (Generic, NFData, Show, Typeable)

makeLenses ''InternalEvent

instance ToJSON   ActionType
instance FromJSON ActionType
instance ToJSON   InternalEvent
instance FromJSON InternalEvent


fromString :: String -> InternalEvent
fromString str = result where
    (commandStr, argStr) = List.break (== ' ') str & _2 %~ drop 1
    result = InternalEvent (read commandStr) argStr
