{-# LANGUAGE DeriveAnyClass #-}
module LunaStudio.Data.Diff where

import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Prologue
import           LunaStudio.Data.Point         (Point)


data Diff = Diff
    { _range    :: Maybe (Point, Point)
    , _newText  :: Text
    , _cursor   :: Maybe Point
    } deriving (Binary, Eq, Generic, NFData, Show)

makeLenses ''Diff
instance FromJSON Diff
instance ToJSON   Diff
