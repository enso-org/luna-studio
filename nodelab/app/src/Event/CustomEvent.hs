module Event.CustomEvent where


import           Data.Aeson        (ToJSON, toJSON)
import           Luna.Studio.Prelude


data Event = RawEvent String JSVal deriving (Generic)

makeLenses ''Event

instance ToJSON Event where
    toJSON (RawEvent topic _) = toJSON $ "Event: " <> topic

instance Show Event where
    show (RawEvent topic _) = show $ "Event: " <> topic
