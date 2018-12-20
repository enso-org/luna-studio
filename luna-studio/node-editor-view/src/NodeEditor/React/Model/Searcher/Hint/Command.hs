{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint.Command where


import Common.Prelude

import qualified NodeEditor.Event.Shortcut     as Shortcut
import qualified Searcher.Engine.Data.Database as Database

import LunaStudio.Data.Searcher.Hint (SearcherHint (documentation, prefix))
import Searcher.Engine.Data.Database (Database, SearcherData (fixedScore, text))



---------------------
-- === Command === --
---------------------


-- === Definition === --

data OtherCommands = AddNode deriving (Bounded, Enum, Eq, Generic, Read, Show)

newtype Command = Command Text deriving (Eq, Generic, Show)

instance NFData Command
instance SearcherData Command where
    text       = to $! \(Command txt) -> txt
    fixedScore = to $! \(Command txt) -> txt ^. fixedScore
instance SearcherHint Command where
    prefix        = to $! const mempty
    documentation = to $! const mempty


-- === API === --

allCommands :: [Command]
allCommands = commands <> otherCommands where
    toCommand :: Show a => a -> Command
    toCommand     = \c -> Command $! convert $! show c
    commands      = fmap toCommand $! [ (minBound :: Shortcut.Command) .. ]
    otherCommands = fmap toCommand $! [  minBound :: OtherCommands ]
{-# INLINE allCommands #-}

database :: Database Command
database = Database.mk allCommands
{-# INLINE database #-}
