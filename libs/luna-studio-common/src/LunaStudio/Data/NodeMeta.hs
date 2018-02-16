module LunaStudio.Data.NodeMeta where

import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.Binary                (Binary)
import           LunaStudio.Data.Position   (Position)
import           LunaStudio.Data.Visualizer (Visualizer)
import           Prologue


data NodeMeta = NodeMeta { _position           :: Position
                         , _displayResult      :: Bool
                         , _selectedVisualizer :: Maybe Visualizer
                         } deriving (Eq, Generic, Ord, Show)

makeLenses ''NodeMeta
instance Binary   NodeMeta
instance NFData   NodeMeta
instance FromJSON NodeMeta
instance ToJSON   NodeMeta

instance Default NodeMeta where
    def = NodeMeta def False def
