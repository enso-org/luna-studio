module Node.Editor.React.Model.ConnectionPen where

import           Data.Aeson             (ToJSON)
import           Node.Editor.Data.Color (Color)
import           Luna.Prelude


data ConnectionPen = ConnectionPen { _path  :: String
                                   , _color :: Color
                                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''ConnectionPen
instance ToJSON ConnectionPen
