module Event.Widget where

import           Data.Aeson               (FromJSON, ToJSON)
import           Luna.Studio.Prelude

import           Empire.API.JSONInstances ()
import           Object.UITypes            (WidgetId)

data Payload = CodeEditorChange
             | CodeEditorBlur
             deriving (Eq, Show, Generic)

makeLenses ''Payload

instance ToJSON   Payload
instance FromJSON Payload


data Event = WidgetEvent { _widgetId :: WidgetId
                         , _payload  :: Payload
                         }
                         deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON   Event
instance FromJSON Event
