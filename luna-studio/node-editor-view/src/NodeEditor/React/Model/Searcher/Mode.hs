{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Mode where

import Common.Prelude

import NodeEditor.React.Model.Searcher.Mode.Node (Node)



------------------
-- === Mode === --
------------------


-- === Definition === --

data Mode
    = Command
    | Node    Node
    deriving (Eq, Generic, Show)

makePrisms ''Mode

instance NFData Mode