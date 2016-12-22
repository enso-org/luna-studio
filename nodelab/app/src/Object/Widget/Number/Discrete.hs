module Object.Widget.Number.Discrete where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude
import           Object.Widget

data DiscreteNumber = DiscreteNumber { _position       :: Position
                                     , _size           :: Size
                                     , _label          :: Text
                                     , _value          :: Int
                                     , _enabled        :: Bool
                                     , _dragStartValue :: Maybe Int
                                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''DiscreteNumber
instance ToJSON DiscreteNumber

create :: Size -> Text -> Int -> DiscreteNumber
create s l v = DiscreteNumber def s l v True def

displayValue' :: DiscreteNumber -> String
displayValue' model = show $ model ^. value

displayValue :: Getter DiscreteNumber String
displayValue = to displayValue'
