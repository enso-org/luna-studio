module Object.Widget.Slider.Discrete where

import           Data.Aeson        (ToJSON)
import           Object.Widget
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

data DiscreteSlider = DiscreteSlider { _position       :: Vector2 Double
                                     , _size           :: Vector2 Double
                                     , _label          :: Text
                                     , _enabled        :: Bool
                                     , _minValue       :: Int
                                     , _maxValue       :: Int
                                     , _value          :: Int
                                     , _dragStartValue :: Maybe Int
                                     } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''DiscreteSlider
instance ToJSON DiscreteSlider

create :: Size -> Text -> Int -> Int -> Int -> DiscreteSlider
create s l min' max' v = DiscreteSlider def s l True min' max' v def

instance IsDisplayObject DiscreteSlider where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True

displayValue' :: DiscreteSlider -> String
displayValue' slider = show $ slider ^. value

displayValue :: Getter DiscreteSlider String
displayValue = to displayValue'

range' :: DiscreteSlider -> Int
range' slider = (slider ^. maxValue) - (slider ^. minValue)

range :: Getter DiscreteSlider Int
range = to range'

getNormValue :: DiscreteSlider -> Double
getNormValue slider = (fromIntegral $ (slider ^. value) - (slider ^. minValue)) / (fromIntegral $ slider ^. range)

setNormValue :: DiscreteSlider -> Double -> DiscreteSlider
setNormValue slider val = slider & value .~ newValue where
    boundedVal = max 0.0 $ min 1.0 val
    newValue = (round $ boundedVal * (fromIntegral $ slider ^. range)) + (slider ^. minValue)

boundedNormValue :: Lens' DiscreteSlider Double
boundedNormValue = lens getNormValue setNormValue

getValue :: DiscreteSlider -> Int
getValue slider = slider ^. value

setValue :: DiscreteSlider -> Int -> DiscreteSlider
setValue slider val = slider & value .~ boundedVal where
    boundedVal = max (slider ^. minValue) $ min (slider ^. maxValue) val

boundedValue :: Lens' DiscreteSlider Int
boundedValue = lens getValue setValue
