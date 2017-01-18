module Luna.Studio.Prelude.Instances where

import           Data.Aeson
import           Development.Placeholders
import           Empire.API.JSONInstances ()
import           React.Flux



instance ToJSON   MouseEvent where
    toJSON _ = toJSON "(MouseEvent)"
instance FromJSON MouseEvent where
    parseJSON = $notImplemented

instance ToJSON   KeyboardEvent where
    toJSON _ = toJSON "(KeyboardEvent)"
instance FromJSON KeyboardEvent where
    parseJSON = $notImplemented

instance ToJSON   WheelEvent where
    toJSON _ = toJSON "(WheelEvent)"
instance FromJSON WheelEvent where
    parseJSON = $notImplemented

instance ToJSON   TouchEvent where
    toJSON _ = toJSON "(TouchEvent)"
instance FromJSON TouchEvent where
    parseJSON = $notImplemented


instance ToJSON   Event where
    toJSON _ = toJSON "(Event)"
instance FromJSON Event where
    parseJSON = $notImplemented
