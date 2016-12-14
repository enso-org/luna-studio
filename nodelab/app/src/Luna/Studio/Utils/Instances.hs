module Luna.Studio.Utils.Instances where

import           Data.Aeson
import           Development.Placeholders
import           React.Flux



instance ToJSON   MouseEvent where
    toJSON _ = toJSON "(MouseEvent)"
instance FromJSON MouseEvent where
    parseJSON = $notImplemented

instance ToJSON   KeyboardEvent where
    toJSON _ = toJSON "(KeyboardEvent)"
instance FromJSON KeyboardEvent where
    parseJSON = $notImplemented

instance ToJSON   Event where
    toJSON _ = toJSON "(Event)"
instance FromJSON Event where
    parseJSON = $notImplemented
