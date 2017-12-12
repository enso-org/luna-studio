{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.Atom where

import           Common.Analytics  (IsTrackedEvent)
import           Common.Data.Event (EventName)
import           Common.Prelude
import           Data.Aeson        (FromJSON, ToJSON)


data Event = SetFile { path :: FilePath }
           | UnsetFile
           | UpdateFilePath { path :: FilePath }
           deriving (Eq, FromJSON, Generic, NFData, Show, ToJSON, Typeable)

instance EventName Event
instance IsTrackedEvent Event
