{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.Atom where

import Common.Prelude

import Common.Analytics  (IsTrackedEvent)
import Common.Data.Event (EventName)
import Data.Aeson        (FromJSON, ToJSON)
import Path              (Path, Rel, File)


data Event = SetFile { path :: Path Rel File }
           | UnsetFile
           | UpdateFilePath { path :: Path Rel File }
           deriving (Eq, FromJSON, Generic, NFData, Show, ToJSON, Typeable)

instance EventName Event
instance IsTrackedEvent Event
