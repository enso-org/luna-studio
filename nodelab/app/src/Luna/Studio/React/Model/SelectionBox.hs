module Luna.Studio.React.Model.SelectionBox where

import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude



data SelectionBox = SelectionBox
    { _visible :: Bool
    , _start   :: Position
    , _end     :: Position
    } deriving (Show)

makeLenses ''SelectionBox

instance Default SelectionBox where
    def = SelectionBox False def def
