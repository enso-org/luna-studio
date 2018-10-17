module Test.Code.MarkerSanitizerSpec (spec) where

import Empire.Prelude

import qualified Empire.Commands.Graph        as Graph

import Empire.Commands.Graph.Code     (sanitizeMarkers)
import LunaStudio.Data.GraphLocation  (filePath, top)
import LunaStudio.Data.Point          (Point (Point))
import LunaStudio.Data.TextDiff       (TextDiff (TextDiff))
import Test.Hspec                     (Spec, describe, it, shouldBe)
import Test.Hspec.Empire              (normalizeLunaCode, testCaseWithMarkers)
-- import Test.Hspec.Expectations.Lifted (shouldBe)
import Text.RawString.QQ              (r)


spec :: Spec
spec = describe "sanitization" $ do
    it "removes marker from accessor without spaces" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = foo«2».bar
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = foo.bar
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "removes marker from strings" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = "foo«2»bar"
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = "foobar"
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "removes marker from None" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                N«1»one
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "removes marker from accessor with spaces" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = foo «2». bar
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = foo . bar
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
        -- it "removes markers inside expressions" $ let
        --     code = [r|
        --         import Std.Base

        --         «0»def main:
        --             «1»node = foobar
        --             None
        --         |]
        --     in testCaseWithMarkers code code $ \(view filePath -> file) -> do
        --         Graph.substituteCodeFromPoints file
        --             [TextDiff (Just (Point 14 3, Point 14 3)) "\n    " Nothing]
        --         Graph.substituteCodeFromPoints file
        --             [TextDiff (Just (Point 14 3, Point 4 4)) "" Nothing]
        -- 