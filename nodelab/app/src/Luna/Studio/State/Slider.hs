{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.State.Slider where

import           Control.DeepSeq         (NFData)
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Position           (Position)
import           Luna.Studio.Prelude

import           Empire.API.Data.PortRef (AnyPortRef)



data State = State { _portRef   :: AnyPortRef
                   , _startPos  :: Position
                   , _initValue :: InitValue
                   } deriving (Eq, Show, Generic)

data InitValue = Discrete  Int
               | Continous Double
               deriving (Eq, Show, Generic, NFData)

makeLenses ''State
makeLenses ''InitValue

instance ToJSON State
instance ToJSON InitValue
instance FromJSON InitValue
