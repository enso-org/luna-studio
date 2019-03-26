{-# LANGUAGE JavaScriptFFI #-}

module Searcher.Data.Database where

import Common.Prelude

import qualified Common.Marshal      as Marshal
import qualified Data.Array          as Array
import qualified Searcher.Data.Class as SearcherData

import Data.Array          (Array)
import Searcher.Data.Class (SearcherData)

----------------------
-- === Database === --
----------------------

-- === Definition === --

data Database a = Database
    { _jsDatabase   :: JSVal
    , _indexMapping :: Array Int a
    } deriving (Functor, Generic)
makeLenses ''Database

instance NFData a => NFData (Database a)

instance SearcherData a => Default (Database a) where
    def = create []

-- === JS Binding === --

foreign import javascript safe "new window.searcherEngine.Database($1)"
    jsCreate :: JSVal -> JSVal

-- === API === --

create :: SearcherData a => [a] -> Database a
create hints = let
    len         = length hints
    hintsArr    = Array.listArray (0, len - 1) hints
    assocs      = (_2 %~ view SearcherData.text) <$> Array.assocs hintsArr
    -- Yes, it's `unsafeToJSVal`. It is a pure computation
    -- by the virtue of its usage and semantics of the JS library,
    -- so we ruthlessly navigate around GHCJS marshaling constraints.
    jsValAssocs = Marshal.unsafeToJSVal assocs
    jsDb        = jsCreate jsValAssocs
    in Database jsDb hintsArr

elems :: Database a -> [a]
elems = Array.elems . view indexMapping

size :: Database a -> Int
size db = let
    (left, right) = Array.bounds $ db ^. indexMapping
    boundsSize = right - left + 1
    in max boundsSize 0
