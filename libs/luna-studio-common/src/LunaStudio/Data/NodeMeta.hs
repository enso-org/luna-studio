{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

module LunaStudio.Data.NodeMeta where

import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.Binary                (Binary)

import           LunaStudio.Data.Position   (Position)
import           LunaStudio.Data.Visualizer (VisualizerId)
import           Prologue


data NodeMeta = NodeMeta
    { _position           :: Position
    , _displayResult      :: Bool
    , _selectedVisualizer :: Maybe VisualizerId
    } deriving (Eq, Generic, Show)

makeLenses ''NodeMeta

instance Ord NodeMeta where
    compare a b = compare (a ^. position) (b ^. position)

instance Default NodeMeta where
    def = NodeMeta def False def

instance Binary   NodeMeta
instance NFData   NodeMeta
instance FromJSON NodeMeta
instance ToJSON   NodeMeta

