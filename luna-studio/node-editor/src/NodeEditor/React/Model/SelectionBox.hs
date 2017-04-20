module NodeEditor.React.Model.SelectionBox where

import           Data.Position       (Position)
import           Common.Prelude



data SelectionBox = SelectionBox
    { _start   :: Position
    , _end     :: Position
    } deriving (Show, Eq)

makeLenses ''SelectionBox

instance Default SelectionBox where
    def = SelectionBox def def
