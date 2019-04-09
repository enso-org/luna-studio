{-# LANGUAGE JavaScriptFFI #-}

module Searcher.Engine where

import Common.Prelude
import Prologue.Unsafe (error)

import qualified Data.Array             as Array
import qualified Data.Map               as Map
import qualified Searcher.Data.Database as Database
import qualified Searcher.Data.Match    as Match
import qualified Searcher.Data.Result   as Result

import GHCJS.Marshal.Pure     (pToJSVal)
import Searcher.Data.Database (Database, JsDatabase)
import Searcher.Data.Match    (MatchSource)
import Searcher.Data.Result   (Result (Result))
import System.IO.Unsafe       (unsafePerformIO)

-- === Internal === --

foreign import javascript safe "$1.query($2)"
    jsQuery :: JSVal -> JSVal -> JSVal

-- | There's a lot of unsafe operations here.
--   First of all, the `error` and a `fromJust` equivalent.
--   This is translating the results from JS FFI, if there's a `Nothing`
--   returned, that means API mismatch and we want to fail soon.
--   Secondly, there's an `unsafePerformIO`. The returned value will
--   never be mutated from the JS side, so we can do it.
castResults :: JSVal -> [([Int], [Int], Double)]
castResults = unsafeUnwrap . unsafePerformIO . fromJSVal where
    errorMsg = unlines [ "Critical Bug in searcher engine."
                       , "API mismatch between JS library and HS binding."
                       ]
    unsafeUnwrap = fromMaybe $ error errorMsg

rawQuery :: MatchSource -> JsDatabase -> Text -> [Result Int]
rawQuery source db q = let
    bareResults  = jsQuery (unwrap db) (pToJSVal q)
    typedResults = castResults bareResults
    mkResult match score ix = Result ix ix score (Match.make source match)
    processResultGroup (ixes, match, score) = mkResult match score <$> ixes
    in concatMap processResultGroup typedResults

rebuildResults :: Database a -> [Result Int] -> [Result a]
rebuildResults db results = let
    ixMapping = db ^. Database.indexMapping
    in (ixMapping Array.!) <<$>> results

-- === Public API === --

selectBestResults :: [Result a] -> [Result a]
selectBestResults results = let
    insertMax map result
        = Map.insertWith Result.maxByScore (result ^. Result.id) result map
    resultsMap = foldl' insertMax Map.empty results
    in Map.elems resultsMap

queryExpressions :: Database a -> Text -> [Result a]
queryExpressions db query = let
    bareResults = rawQuery Match.Expression (db ^. Database.jsDatabase) query
    in rebuildResults db bareResults

queryTags :: Database a -> Text -> [Result a]
queryTags db query = let
    bareResults = rawQuery Match.Tag (db ^. Database.jsTagsDatabase) query
    in rebuildResults db bareResults
