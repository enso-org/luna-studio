{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.Searcher where

import           Common.Prelude
import           Data.Aeson     (FromJSON)
import           React.Flux     (KeyboardEvent)



data Event = InputChanged Text Int Int
           | Accept
           | AcceptInput
           | AcceptEntry Int
           | TabPressed
           | MoveDown
           | MoveUp
           | MoveLeft
           | KeyDown KeyboardEvent
           | KeyUp   KeyboardEvent
            deriving (FromJSON, Generic, NFData, Show, Typeable)
