module React.Store.SelectionBox where

import           Utils.PreludePlus
import           Utils.Vector



data SelectionBox = SelectionBox
    { _visible :: Bool
    , _start   :: Vector2 Double
    , _end     :: Vector2 Double
    } deriving (Show)

makeLenses ''SelectionBox

instance Default SelectionBox where
    def = SelectionBox False def def
