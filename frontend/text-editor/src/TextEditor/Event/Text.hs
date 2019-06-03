{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Text where

import Common.Prelude

import qualified LunaStudio.Data.GraphLocation as GraphLocation

import Common.Analytics              (IsTrackedEvent)
import Common.Data.Event             (EventName)
import Data.Aeson                    (FromJSON, ToJSON)
import LunaStudio.Data.GraphLocation (GraphLocation)
import LunaStudio.Data.TextDiff      (TextDiff)
import Path                          (File, Path, Rel)


data TextEvent = TextEvent
        { _location  :: GraphLocation
        , _diffs     :: [TextDiff]
        } deriving (FromJSON, Generic, NFData, Show, ToJSON, Typeable)

makeLenses ''TextEvent

filePath :: Lens' TextEvent (Path Rel File)
filePath = location . GraphLocation.filePath

instance EventName TextEvent
instance IsTrackedEvent TextEvent
