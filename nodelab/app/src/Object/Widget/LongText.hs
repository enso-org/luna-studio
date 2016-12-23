module Object.Widget.LongText where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude
import           Object.Widget

data TextAlignment = Left | Center | Right deriving (Eq, Show, Generic)
instance ToJSON TextAlignment

data Type = Text | Code deriving (Eq, Show, Generic)
instance ToJSON Type

data LongText = LongText { _position  :: Position
                         , _size      :: Size
                         , _value     :: Text
                         , _alignment :: TextAlignment
                         , _tpe       :: Type
                         } deriving (Eq, Show, Typeable, Generic)

makeLenses ''LongText
instance ToJSON LongText

create :: Size -> Text -> TextAlignment -> Type -> LongText
create = LongText def
