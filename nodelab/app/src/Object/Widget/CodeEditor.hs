module Object.Widget.CodeEditor where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2 (Vector2))
import           Luna.Studio.Prelude
import           Object.Widget

data CodeEditor = CodeEditor { _position  :: Position
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
