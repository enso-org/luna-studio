module Utils.Instances where

import           Data.Aeson
import           Development.Placeholders
import           Prologue
import           React.Flux



instance ToJSON   MouseEvent where
    toJSON _ = toJSON "(MouseEvent)"
instance FromJSON MouseEvent where
    parseJSON = $notImplemented
