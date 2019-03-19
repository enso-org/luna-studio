{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint.Command where


import Common.Prelude

import qualified NodeEditor.Event.Shortcut     as Shortcut
import qualified Searcher.Data.Database        as Database

import Searcher.Data.Class (SearcherData (text),
                            SearcherHint (documentation, prefix))



---------------------
-- === Command === --
---------------------

-- === Definition === --

data OtherCommands = AddNode deriving (Bounded, Enum, Eq, Generic, Read, Show)

newtype Command = Command Text deriving (Eq, Generic, Show)

instance NFData Command
instance SearcherData Command where
    text       = to $ \(Command txt) -> txt
instance SearcherHint Command where
    prefix        = to $ const mempty
    documentation = to $ const mempty


-- === API === --

allCommands :: [Command]
allCommands = commands <> otherCommands where
    toCommand :: Show a => a -> Command
    toCommand     = \c -> Command . convert $ show c
    commands      = fmap toCommand [ (minBound :: Shortcut.Command) .. ]
    otherCommands = fmap toCommand [  minBound :: OtherCommands ]
{-# INLINE allCommands #-}

database :: Database.Database Command
database = Database.create allCommands
{-# INLINE database #-}
