module Luna.Studio.React.Model.ConnectionPen where

import           Data.Aeson             (ToJSON)
import           Luna.Studio.Data.Color (Color)
import           Luna.Studio.Prelude


data ConnectionPen = ConnectionPen { _path  :: String
                                   , _color :: Color
                                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''ConnectionPen
instance ToJSON ConnectionPen
