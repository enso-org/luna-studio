{-# LANGUAGE JavaScriptFFI #-}

module Searcher.Data.Database where

import Common.Prelude

import qualified Data.Array as Array

import Data.Array          (Array)
import Searcher.Data.Class (SearcherData (text))
import System.IO.Unsafe    (unsafePerformIO)

data Database a = Database
    { _jsDatabase   :: JSVal
    , _indexMapping :: Array Int a
    } deriving Generic
makeLenses ''Database

instance NFData a => NFData (Database a)

instance SearcherData a => Default (Database a) where
    def = create []

foreign import javascript safe "new window.searcherEngine.Database($1)"
    jsCreate :: JSVal -> JSVal

create :: SearcherData a => [a] -> Database a
create hints = let
    len         = length hints
    hintsArr    = Array.listArray (0, len - 1) hints
    assocs      = (_2 %~ view text) <$> Array.assocs hintsArr
    -- Yes, it's an `unsafePerformIO`. It is a pure computation
    -- by the virtue of its usage and semantics of the JS library,
    -- so we ruthlessly navigate around GHCJS marshaling constraints.
    jsValAssocs = unsafePerformIO $ toJSVal assocs
    jsDb        = jsCreate jsValAssocs
    in Database jsDb hintsArr

elems :: Database a -> [a]
elems = Array.elems . view indexMapping
