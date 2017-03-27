module Luna.Studio.React.Model.SelectionBox where

import           Data.Position       (Position)
import           Luna.Studio.Prelude



data SelectionBox = SelectionBox
    { _start   :: Position
    , _end     :: Position
    } deriving (Show, Eq)

makeLenses ''SelectionBox

instance Default SelectionBox where
    def = SelectionBox def def