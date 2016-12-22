module Object.Widget.Number.Continuous where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude
import           Numeric
import           Object.Widget

data ContinuousNumber =  ContinuousNumber { _position       :: Position
                                          , _size           :: Size
                                          , _label          :: Text
                                          , _value          :: Double
                                          , _enabled        :: Bool
                                          , _dragStartValue :: Maybe Double
                                          } deriving (Eq, Show, Typeable, Generic)

makeLenses ''ContinuousNumber
instance ToJSON ContinuousNumber

create :: Size -> Text -> Double -> ContinuousNumber
create s l v = ContinuousNumber def s l v True def

displayValue' :: ContinuousNumber -> String
displayValue' model = showGFloatAlt (Just 2) (model ^. value) ""

displayValue :: Getter ContinuousNumber String
displayValue = to displayValue'
