{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.Searcher where

import           Common.Data.Event (EventName)
import           Common.Prelude
import           Data.Aeson        (FromJSON)
import           React.Flux        (KeyboardEvent)


data Event
    = InputChanged Text Int Int
    | Accept
    | AcceptInput
    | HintShortcut   Int
    | AcceptWithHint Int
    | TabPressed
    | MoveDown
    | MoveUp
    | MoveLeft
    | KeyDown KeyboardEvent
    | KeyUp   KeyboardEvent
    | ParenOpen
    | ParenClose
    | Backspace
    | Undo
    | Redo
    | SelectionChanged
    deriving (FromJSON, Generic, NFData, Show, Typeable)

instance EventName Event
