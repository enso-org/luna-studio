{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint where

import Common.Prelude

import qualified LunaStudio.Data.Searcher.Hint as Hint
import qualified Searcher.Data.Result          as Result

import NodeEditor.React.Model.Searcher.Hint.Command (Command)
import NodeEditor.React.Model.Searcher.Hint.Node    (Node)
import Searcher.Data.Class                          (SearcherData (text),
                                                     SearcherHint
                                                        (prefix, documentation))
import Searcher.Data.Result                         (Result)



------------------
-- === Hint === --
------------------


-- === Definition === --

data Hint
    = Command Command
    | Node    Node
    deriving (Eq, Generic, Show)

makePrisms ''Hint

instance NFData Hint
instance SearcherData Hint where
    text = to $! \case
        Command h -> h ^. text
        Node    h -> h ^. text

instance SearcherHint Hint where
    prefix = to $! \case
        Command h -> h ^. prefix
        Node    h -> h ^. prefix
    documentation = to $! \case
        Command h -> h ^. documentation
        Node    h -> h ^. documentation
