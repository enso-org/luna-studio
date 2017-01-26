module Empire.API.Data.NodeMeta where

import           Data.Binary (Binary)
import           Prologue

data NodeMeta = NodeMeta { _position      :: (Double, Double)
                         , _displayResult :: Bool
                         } deriving (Eq, Generic, NFData, Ord, Show)

makeLenses ''NodeMeta

instance Default NodeMeta where
    def = NodeMeta def True

instance Binary NodeMeta
