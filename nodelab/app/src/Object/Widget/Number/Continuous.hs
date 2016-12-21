module Object.Widget.Number.Continuous where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2)
import           Luna.Studio.Prelude
import           Numeric
import           Object.Widget

data ContinuousNumber =  ContinuousNumber { _position       :: Position
                                          , _size           :: Vector2 Double
                                          , _label          :: Text
                                          , _value          :: Double
                                          , _enabled        :: Bool
                                          , _dragStartValue :: Maybe Double
                                          } deriving (Eq, Show, Typeable, Generic)

makeLenses ''ContinuousNumber
instance ToJSON ContinuousNumber

create :: Size -> Text -> Double -> ContinuousNumber
create s l v = ContinuousNumber def s l v True def

instance IsDisplayObject ContinuousNumber where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True

displayValue' :: ContinuousNumber -> String
displayValue' model = showGFloatAlt (Just 2) (model ^. value) ""

displayValue :: Getter ContinuousNumber String
displayValue = to displayValue'
