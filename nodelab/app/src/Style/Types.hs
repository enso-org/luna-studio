module Style.Types where

import           Data.Aeson        (ToJSON)
import           Luna.Studio.Prelude

data Color   = Color { _r :: Double
                     , _g :: Double
                     , _b :: Double
                     , _a :: Double
                     } deriving (Show, Eq, Generic)

transparent = Color 0.0 0.0 0.0 0.0

instance ToJSON Color

data Padding = Padding { _top    :: Double
                       , _right  :: Double
                       , _bottom :: Double
                       , _left   :: Double
                       } deriving (Show, Eq, Generic)

instance Default Padding where
    def = Padding 0.0 0.0 0.0 0.0


uniformPadding a = Padding a a a a
xyPadding x y    = Padding y x y x
