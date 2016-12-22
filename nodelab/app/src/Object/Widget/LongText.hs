module Object.Widget.LongText where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2)
import           Luna.Studio.Prelude
import           Object.Widget

data TextAlignment = Left | Center | Right deriving (Eq, Show, Generic)
instance ToJSON TextAlignment

data Type = Text | Code deriving (Eq, Show, Generic)
instance ToJSON Type

data LongText = LongText { _position  :: Position
                         , _size      :: Vector2 Double
                         , _value     :: Text
                         , _alignment :: TextAlignment
                         , _tpe       :: Type
                         } deriving (Eq, Show, Typeable, Generic)

makeLenses ''LongText
instance ToJSON LongText

create :: Size -> Text -> TextAlignment -> Type -> LongText
create = LongText def

instance IsDisplayObject LongText where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
