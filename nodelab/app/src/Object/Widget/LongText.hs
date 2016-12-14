module Object.Widget.LongText where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector
import           Object.Widget
import           Data.Aeson (ToJSON)

data TextAlignment = Left | Center | Right deriving (Eq, Show, Generic)
instance ToJSON TextAlignment

data Type = Text | Code deriving (Eq, Show, Generic)
instance ToJSON Type

data LongText = LongText { _position  :: Vector2 Double
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
