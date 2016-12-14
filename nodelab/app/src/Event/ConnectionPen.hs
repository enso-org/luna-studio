module Event.ConnectionPen where


import Luna.Studio.Prelude
import Object.UITypes
import Data.Aeson (ToJSON)


data Event = Segment { _widgets :: [WidgetId] } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Event

instance ToJSON Event
