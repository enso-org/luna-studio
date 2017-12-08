{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Text where

import           Common.Analytics              (IsTrackedEvent)
import           Common.Prelude
import           Common.Data.Event             (EventName)
import           Data.Aeson                    (FromJSON, ToJSON)
import           LunaStudio.Data.Diff          (Diff)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import qualified LunaStudio.Data.GraphLocation as GraphLocation


data TextEvent = TextEvent
        { _location  :: GraphLocation
        , _diffs     :: [Diff]
        } deriving (FromJSON, Generic, NFData, Show, ToJSON, Typeable)

makeLenses ''TextEvent

filePath :: Lens' TextEvent FilePath
filePath = location . GraphLocation.filePath

instance EventName TextEvent
instance IsTrackedEvent TextEvent
