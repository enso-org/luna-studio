module React.Model.SelectionBox where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector



data SelectionBox = SelectionBox
    { _visible :: Bool
    , _start   :: Vector2 Int
    , _end     :: Vector2 Int
    } deriving (Show)

makeLenses ''SelectionBox

instance Default SelectionBox where
    def = SelectionBox False def def
