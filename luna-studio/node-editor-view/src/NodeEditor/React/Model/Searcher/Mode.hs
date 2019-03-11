{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Mode where

import Common.Prelude

import qualified NodeEditor.React.Model.Searcher.Mode.Node as Node

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

isExpressionSearcher :: Mode -> Bool
isExpressionSearcher = has $ _Node . Node.mode . Node._ExpressionMode
