{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Event.Atom where

import           Common.Prelude
import           Data.Aeson     (FromJSON, ToJSON)


data Event = OpenFile FilePath
           | CloseFile
           deriving (Eq, Generic, NFData, Read, Show, Typeable)

instance ToJSON   Event
instance FromJSON Event
