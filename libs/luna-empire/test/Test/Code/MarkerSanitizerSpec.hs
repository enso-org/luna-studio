module Test.Code.MarkerSanitizerSpec (spec) where

import Empire.Prelude

import Empire.Commands.Graph.Code (sanitizeMarkers)
import Test.Hspec                 (Spec, describe, it, parallel, shouldBe)
import Test.Hspec.Empire          (normalizeLunaCode)
import Text.RawString.QQ          (r)


spec :: Spec
spec = parallel $ describe "sanitization" $ do
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
    it "removes marker at the end of the line" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»foo = 1«10»
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»foo = 1
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
    it "removes marker from accessor with spaces 2" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = foo .«2» bar
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = foo . bar
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "removes two consecutive markers" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»«2»node = foo . bar
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = foo . bar
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "removes three consecutive markers" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»«2»«3»node = foo . bar
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = foo . bar
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "leaves marker inside lambda" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x: y: «2»x + y
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` initialCode
    it "removes marker after lambda variable" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x«2»: x + x
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x: x + x
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "works on empty string"  $ sanitizeMarkers ""       `shouldBe` ""
    it "works on single marker" $ sanitizeMarkers "«2»"    `shouldBe` "«2»"
    it "works on double marker" $ sanitizeMarkers "«2»«3»" `shouldBe` "«2»"
    it "works on double marker with spaces" $
        sanitizeMarkers "«2»   «3»a" `shouldBe` "«2»   a"
