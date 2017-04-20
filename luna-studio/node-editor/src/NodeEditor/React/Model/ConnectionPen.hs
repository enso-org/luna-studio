module NodeEditor.React.Model.ConnectionPen where

import           Data.Aeson             (ToJSON)
import           NodeEditor.Data.Color (Color)
import           Common.Prelude


data ConnectionPen = ConnectionPen { _path  :: String
                                   , _color :: Color
                                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''ConnectionPen
instance ToJSON ConnectionPen
