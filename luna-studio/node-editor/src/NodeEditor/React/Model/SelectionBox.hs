{-# LANGUAGE StrictData #-}
module NodeEditor.React.Model.SelectionBox where

import           Common.Prelude
import           LunaStudio.Data.Position (Position)



data SelectionBox = SelectionBox
    { _start   :: Position
    , _end     :: Position
    } deriving (Show, Eq)

makeLenses ''SelectionBox

instance Default SelectionBox where
    def = SelectionBox def def
