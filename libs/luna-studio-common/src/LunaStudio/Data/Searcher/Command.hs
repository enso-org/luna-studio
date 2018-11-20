module LunaStudio.Data.Searcher.Command
    ( module LunaStudio.Data.Searcher.Command
    , module X
    ) where

import Searcher.Engine as X hiding (search)

import Prologue

import qualified Searcher.Engine              as Searcher
import qualified Searcher.Engine.Data.Command as Command


search :: Query -> [Command.Name] -> [Match Command]
search q = Searcher.search q . fmap (flip Command def)
