module Object.Widget.Number.Discrete where

import           Data.Aeson        (ToJSON)
import           Object.Widget
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

data DiscreteNumber = DiscreteNumber { _position       :: Vector2 Double
                                     , _size           :: Vector2 Double
                                     , _label          :: Text
                                     , _value          :: Int
                                     , _enabled        :: Bool
                                     , _dragStartValue :: Maybe Int
                                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''DiscreteNumber
instance ToJSON DiscreteNumber

create :: Size -> Text -> Int -> DiscreteNumber
create s l v = DiscreteNumber def s l v True def

instance IsDisplayObject DiscreteNumber where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True

displayValue' :: DiscreteNumber -> String
displayValue' model = show $ model ^. value

displayValue :: Getter DiscreteNumber String
displayValue = to displayValue'
