module Object.Widget.CodeEditor where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude
import           Object.Widget

data CodeEditor = CodeEditor { _position  :: Position
                             , _size      :: Size
                             , _value     :: Text
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CodeEditor
instance ToJSON CodeEditor

create :: Size -> Text -> CodeEditor
create = CodeEditor def
