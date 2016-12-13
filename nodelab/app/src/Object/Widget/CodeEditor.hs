module Object.Widget.CodeEditor where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector
import           Object.Widget
import           Data.Aeson (ToJSON)

data CodeEditor = CodeEditor { _position  :: Vector2 Double
                             , _size      :: Vector2 Double
                             , _value     :: Text
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CodeEditor
instance ToJSON CodeEditor

create :: Size -> Text -> CodeEditor
create = CodeEditor def

instance IsDisplayObject CodeEditor where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
