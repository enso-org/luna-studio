{-# LANGUAGE StrictData #-}
module NodeEditor.React.Model.ConnectionPen where

import           Common.Prelude
import           NodeEditor.Data.Color (Color)


data ConnectionPen = ConnectionPen { _path  :: String
                                   , _color :: Color
                                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''ConnectionPen
