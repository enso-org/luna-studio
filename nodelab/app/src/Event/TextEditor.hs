module Event.TextEditor where


import           Data.Aeson        (ToJSON)
import           Luna.Studio.Prelude

data Event = CodeModified { _code :: Text } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Event

instance ToJSON Event
