module NodeEditor.React.Model.Searcher.Mode.Command
    ( module NodeEditor.React.Model.Searcher.Mode.Command
    , module X
    ) where

import LunaStudio.Data.Searcher.Command as X (Command, Name, search)

import qualified NodeEditor.Event.Shortcut as Shortcut

import Common.Prelude


data OtherCommands = AddNode deriving (Bounded, Enum, Eq, Generic, Read, Show)

allCommands :: [Name]
allCommands = convert <$> (commands <> otherCommands) where
    commands      = show <$> [(minBound :: Shortcut.Command) ..]
    otherCommands = show <$> [(minBound :: OtherCommands)]
