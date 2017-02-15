{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  (when)
import           Data.Default
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           System.Exit

import           Test.Framework                 hiding (Test)
import           Test.Framework.Providers.HUnit
import           Test.HUnit


import           Text.ScopeSearcher.Item
import           Text.ScopeSearcher.QueryResult
import qualified Text.ScopeSearcher.Scope       as Scope
import qualified Text.ScopeSearcher.Searcher    as Searcher

import qualified Mock.Mix                       as Mock


-- Helpers

assertMsg :: String -> String -> Int -> String
assertMsg query sugg ind = "'" <> sugg <> "' should be " <> show ind <> " on suggestions list for '" <> query <> "' query"

prepareTest :: String -> ([QueryResult ()], String -> Int -> String)
prepareTest query = (Scope.searchInScope Mock.items $ Text.pack query, assertMsg query)

assertMatch :: String -> QueryResult -> QueryResult -> Assertion
assertMatch msg expQR resQR = assertEqual msg expQR (resQR { _score = def })

-- Query helpers

queryResult :: Text -> Text -> [Highlight] -> Text -> QueryResult
queryResult modl name hl tpe = QueryResult modl name (Scope.appendPath modl name) hl tpe def

functionType = "function"
moduleType   = "module"

-- Tests

testSearchApp :: Test
testSearchApp = let (suggestions, msg) = prepareTest "app" in TestCase $ do
    assertMatch (msg "APP"    0) (queryResult ""     "app"    [ Highlight 0 3 ] functionType) $ suggestions !! 0
    assertMatch (msg "APPend" 1) (queryResult "List" "append" [ Highlight 0 3 ] functionType) $ suggestions !! 1

testSearchCos :: Test
testSearchCos = let (suggestions, msg) = prepareTest "cos" in TestCase $ do
    assertMatch (msg "COS"   0) (queryResult "Double" "cos"   [ Highlight 0 3 ] functionType) $ suggestions !! 0
    assertMatch (msg "COSh"  1) (queryResult "Double" "cosh"  [ Highlight 0 3 ] functionType) $ suggestions !! 1
    assertMatch (msg "aCOS"  2) (queryResult "Double" "acos"  [ Highlight 1 3 ] functionType) $ suggestions !! 2
    assertMatch (msg "aCOSh" 3) (queryResult "Double" "acosh" [ Highlight 1 3 ] functionType) $ suggestions !! 3
    assertMatch (msg "COnSt" 4) (queryResult ""       "const" [ Highlight 0 2, Highlight 3 1 ] functionType) $ suggestions !! 4

testSearchDou :: Test
testSearchDou = let (suggestions, msg) = prepareTest "dou" in TestCase $ do
    assertMatch (msg "DOUble"   0) (queryResult ""    "Double"   [ Highlight 0 3 ] moduleType)   $ suggestions !! 0
    assertMatch (msg "toDOUble" 1) (queryResult "Int" "toDouble" [ Highlight 2 3 ] functionType) $ suggestions !! 1

testSearchAe :: Test
testSearchAe = let (suggestions, msg) = prepareTest "ae" in TestCase $ do
    assertMatch (msg "AppEnd"   0) (queryResult   "List" "append"   [ Highlight 0 1, Highlight 3 1 ] functionType) $ suggestions !! 0
    assertMatch (msg "tAkE"     1) (queryResult   "List" "take"     [ Highlight 1 1, Highlight 3 1 ] functionType) $ suggestions !! 1
    assertMatch (msg "tAkE"     2) (queryResult "String" "take"     [ Highlight 1 1, Highlight 3 1 ] functionType) $ suggestions !! 2
    assertMatch (msg "nEgAte"   3) (queryResult "Double" "negate"   [ Highlight 3 1, Highlight 5 1 ] functionType) $ suggestions !! 3
    assertMatch (msg "nEgAte"   4) (queryResult    "Int" "negate"   [ Highlight 3 1, Highlight 5 1 ] functionType) $ suggestions !! 4
    assertMatch (msg "reAdFilE" 5) (queryResult       "" "readFile" [ Highlight 2 1, Highlight 7 1 ] functionType) $ suggestions !! 5

testSearchCo :: Test
testSearchCo = let (suggestions, msg) = prepareTest "co" in TestCase $ do
    assertMatch (msg "COs"   0) (queryResult "Double" "cos"   [ Highlight 0 2 ] functionType) $ suggestions !! 0
    assertMatch (msg "COmp"  1) (queryResult ""       "comp"  [ Highlight 0 2 ] functionType) $ suggestions !! 1
    assertMatch (msg "COnst" 2) (queryResult ""       "const" [ Highlight 0 2 ] functionType) $ suggestions !! 2
    assertMatch (msg "COsh"  3) (queryResult "Double" "cosh"  [ Highlight 0 2 ] functionType) $ suggestions !! 3
    assertMatch (msg "aCOs"  4) (queryResult "Double" "acos"  [ Highlight 1 2 ] functionType) $ suggestions !! 4
    assertMatch (msg "aCOsh" 5) (queryResult "Double" "acosh" [ Highlight 1 2 ] functionType) $ suggestions !! 5

testSearchTr :: Test
testSearchTr = let (suggestions, msg) = prepareTest "tr" in TestCase $ do
    assertMatch (msg "sTRing"    0) (queryResult      ""  "String"    [ Highlight 1 2] moduleType)   $ suggestions !! 0
    assertMatch (msg "TostRing"  1) (queryResult   "Bool" "toString"  [ Highlight 0 1, Highlight 4 1] functionType) $ suggestions !! 1
    assertMatch (msg "TostRing"  2) (queryResult "Double" "toString"  [ Highlight 0 1, Highlight 4 1] functionType) $ suggestions !! 2
    assertMatch (msg "TostRing"  3) (queryResult    "Int" "toString"  [ Highlight 0 1, Highlight 4 1] functionType) $ suggestions !! 3
    assertMatch (msg "TostRing"  4) (queryResult "String" "toString"  [ Highlight 0 1, Highlight 4 1] functionType) $ suggestions !! 4
    assertMatch (msg "filTeR"    5) (queryResult      ""  "filter"    [ Highlight 3 1, Highlight 5 1] functionType) $ suggestions !! 5
    assertMatch (msg "filTeR"    6) (queryResult   "List" "filter"    [ Highlight 3 1, Highlight 5 1] functionType) $ suggestions !! 6
    assertMatch (msg "hisTogRam" 7) (queryResult      ""  "histogram" [ Highlight 3 1, Highlight 6 1] functionType) $ suggestions !! 7


-- tests :: IO ()
tests = do
    hUnitTestToTests $ TestList [
          testSearchApp
        , testSearchCos
        , testSearchDou
        , testSearchAe
        , testSearchCo
        , testSearchTr
        ]

main = defaultMain tests
