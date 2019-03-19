{-# LANGUAGE JavaScriptFFI #-}

module Searcher.Engine where

import Common.Prelude
import Prologue.Unsafe (error)

import qualified Data.Array          as Array
import qualified Searcher.Data.Match as Match

import GHCJS.Marshal.Pure     (pToJSVal)
import Searcher.Data.Database (Database, indexMapping, jsDatabase)
import Searcher.Data.Result   (Result (Result))
import System.IO.Unsafe       (unsafePerformIO)

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
    errorMsg = unlines $ [ "Critical Bug in searcher engine."
                         , "API mismatch between JS library and HS binding."
                         ]
    unsafeUnwrap = fromMaybe $ error errorMsg

query :: Database a -> Text -> [Result a]
query db q = let
    bareResults = jsQuery (db ^. jsDatabase) (pToJSVal q)
    typedResults = castResults bareResults
    mkResult match score ix = Result ix score (Match.fromList match)
    processResultGroup (ixes, match, score) = mkResult match score <$> ixes
    intResults = concatMap processResultGroup typedResults
    ixMap = db ^. indexMapping
    results = (ixMap Array.!) <<$>> intResults
    in results
