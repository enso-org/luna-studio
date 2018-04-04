{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Text where

import           Common.Analytics              (IsTrackedEvent)
import           Common.Data.Event             (EventName)
import           Common.Prelude
import           Data.Aeson                    (FromJSON, ToJSON)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import           LunaStudio.Data.TextDiff      (TextDiff)


data TextEvent = TextEvent
        { _location  :: GraphLocation
        , _diffs     :: [TextDiff]
        } deriving (FromJSON, Generic, NFData, Show, ToJSON, Typeable)

makeLenses ''TextEvent

filePath :: Lens' TextEvent FilePath
filePath = location . GraphLocation.filePath

instance EventName TextEvent
instance IsTrackedEvent TextEvent
