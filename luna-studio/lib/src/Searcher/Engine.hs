{-# LANGUAGE JavaScriptFFI #-}

module Searcher.Engine where

import Common.Prelude

import qualified Data.Array as Array

import GHCJS.Marshal.Pure     (pToJSVal)
import Searcher.Data.Database (Database, jsDatabase, indexMapping)
import Searcher.Data.Result   (Result (Result), Match (Match))
import System.IO.Unsafe       (unsafePerformIO)

foreign import javascript safe "$1.query($2)"
    jsQuery :: JSVal -> JSVal -> JSVal

-- | Yes, there's an `unsafeFromJust` here. This is translating the results
--   from JS FFI, if there's a `Nothing` returned, that means API mismatch
--   and the whole world is on fire and we want to fail soon.
--   Also yes, there's an `unsafePerformIO`. The returned value will
--   never be mutated, thus we can do it.
castResults :: JSVal -> [([Int], [Int], Double)]
castResults = unsafeFromJust . unsafePerformIO . fromJSVal

query :: Database a -> Text -> [Result a]
query db q = let
    bareResults = jsQuery (db ^. jsDatabase) (pToJSVal q)
    typedResults = castResults bareResults
    mkResult match score ix = Result ix score (Match match)
    processResultGroup (ixes, match, score) = mkResult match score <$> ixes
    intResults = concatMap processResultGroup typedResults
    ixMap = db ^. indexMapping
    results = (ixMap Array.!) <<$>> intResults
    in results
