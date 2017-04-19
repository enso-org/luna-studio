{-# LANGUAGE DeriveAnyClass #-}
module Luna.Atom.Event.CustomEvent where

import           Data.Aeson          (ToJSON, toJSON)
import           Luna.Prelude


data Event = RawEvent String JSVal deriving (Generic, NFData)

makeLenses ''Event

instance ToJSON Event where
    toJSON (RawEvent topic _) = toJSON $ "Event: " <> topic

instance Show Event where
    show (RawEvent topic _) = show $ "Event: " <> topic
