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

newtype JsDatabase = JsDatabase JSVal deriving (Generic)
makeWrapped ''JsDatabase

instance NFData JsDatabase

data Database a = Database
    { _jsDatabase     :: JsDatabase
    , _jsTagsDatabase :: JsDatabase
    , _indexMapping   :: Array Int a
    } deriving (Functor, Generic)
makeLenses ''Database

instance NFData a => NFData (Database a)

instance SearcherData a => Default (Database a) where
    def = create []

-- === JS Binding === --

foreign import javascript safe "new window.searcherEngine.Database($1)"
    jsCreate :: JSVal -> JSVal

buildJsDatabase :: [(Int, Text)] -> JsDatabase
buildJsDatabase assocs = let
    -- Yes, it's `unsafeToJSVal`. It is a pure computation
    -- by the virtue of its usage and semantics of the JS library,
    -- so we ruthlessly navigate around GHCJS marshaling constraints.
    jsValAssocs = Marshal.unsafeToJSVal assocs
    jsDb        = jsCreate jsValAssocs
    in wrap jsDb

-- === API === --

create :: SearcherData a => [a] -> Database a
create hints = let
    len         = length hints
    hintsArr    = Array.listArray (0, len - 1) hints
    assocs      = Array.assocs hintsArr
    nameAssocs  = (_2 %~ view SearcherData.text) <$> assocs
    toTagsAssocs idx item = (idx,) <$> item ^. SearcherData.tags
    tagsAssocs  = concatMap (uncurry toTagsAssocs) assocs
    jsDb        = buildJsDatabase nameAssocs
    jsTagsDb    = buildJsDatabase tagsAssocs
    in Database jsDb jsTagsDb hintsArr

elems :: Database a -> [a]
elems = Array.elems . view indexMapping

assocs :: Database a -> [(Int, a)]
assocs = Array.assocs . view indexMapping

size :: Database a -> Int
size db = let
    (left, right) = Array.bounds $ db ^. indexMapping
    boundsSize = right - left + 1
    in max boundsSize 0
