{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint where

import Common.Prelude

import qualified LunaStudio.Data.Searcher.Hint as Hint
import qualified Searcher.Engine.Data.Result   as Result

import LunaStudio.Data.Searcher.Hint                (SearcherHint (documentation, prefix))
import NodeEditor.React.Model.Searcher.Hint.Command (Command)
import NodeEditor.React.Model.Searcher.Hint.Node    (Node)
import Searcher.Engine.Data.Database                (SearcherData (fixedScore, text))
import Searcher.Engine.Data.Result                  (Result)



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
    fixedScore = to $! \case
        Command h -> h ^. fixedScore
        Node    h -> h ^. fixedScore

instance SearcherHint Hint where
    prefix = to $! \case
        Command h -> h ^. prefix
        Node    h -> h ^. prefix
    documentation = to $! \case
        Command h -> h ^. documentation
        Node    h -> h ^. documentation

instance SearcherHint a => SearcherHint (Result a) where
    prefix        = Result.hint . prefix
    documentation = Result.hint . documentation
