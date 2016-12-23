module Object.Widget.Slider.Continuous where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude
import           Numeric
import           Object.Widget

data ContinuousSlider = ContinuousSlider { _position       :: Position
                                         , _size           :: Size
                                         , _label          :: Text
                                         , _enabled        :: Bool
                                         , _minValue       :: Double
                                         , _maxValue       :: Double
                                         , _value          :: Double
                                         , _dragStartValue :: Maybe Double
                                         } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''ContinuousSlider
instance ToJSON ContinuousSlider

create :: Size -> Text -> Double -> Double -> Double -> ContinuousSlider
create s l minVal maxVal v = ContinuousSlider def s l True minVal maxVal v def

displayValue' :: ContinuousSlider -> String
displayValue' slider = showFFloat (Just $ precision) val "" where
    val             = slider ^. value
    precision       = sliderPrecision slider

displayValue :: Getter ContinuousSlider String
displayValue = to displayValue'

sliderPrecision :: ContinuousSlider -> Int
sliderPrecision s = displayPrecision (s ^. minValue) (s ^. maxValue)

displayPrecision :: Double -> Double -> Int
displayPrecision minV maxV = numDigits $ ceiling $ max (alog10 minV) (alog10 maxV) where
    alog10 n               = logBase 10 $ abs n
    numDigits n            = max (3 - n) 0 where

range' :: ContinuousSlider -> Double
range' slider = (slider ^. maxValue) - (slider ^. minValue)

range :: Getter ContinuousSlider Double
range = to range'

getNormValue :: ContinuousSlider -> Double
getNormValue slider = ((slider ^. value) - (slider ^. minValue)) / (slider ^. range)

setNormValue :: ContinuousSlider -> Double -> ContinuousSlider
setNormValue slider val = slider & value .~ newValue where
    boundedVal = max 0.0 $ min 1.0 val
    newValue     = boundedVal * (slider ^. range) + (slider ^. minValue)

boundedNormValue :: Lens' ContinuousSlider Double
boundedNormValue = lens getNormValue setNormValue

getValue :: ContinuousSlider -> Double
getValue slider = slider ^. value

setValue :: ContinuousSlider -> Double -> ContinuousSlider
setValue slider val = slider & value .~ boundedVal where
    boundedVal = max (slider ^. minValue) $ min (slider ^. maxValue) val

boundedValue :: Lens' ContinuousSlider Double
boundedValue = lens getValue setValue
