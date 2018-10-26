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
    it "removes marker on two-line broken expression" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = 3
                    «2».+ 2
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = 3
                    .+ 2
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "leaves marker on one-line lambda" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x: «2»x.+ 2
                «3»foo = 1
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x: «2»x.+ 2
                «3»foo = 1
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "leaves marker after unindented expression" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = 1
            «2»foo = 1
                «3»bar = 1
                «4»baz = 1
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = 1
            «2»foo = 1
                «3»bar = 1
                «4»baz = 1
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "leaves marker on two-line lambda" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x:
                    «2»x.+ 2
                «3»foo = 1
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x:
                    «2»x.+ 2
                «3»foo = 1
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "leaves marker on multi-line lambda" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x:
                    «2»c = x.+ 2
                    «3»d = x.* 3
                    «4»c + d
                «5»foo = 1
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x:
                    «2»c = x.+ 2
                    «3»d = x.* 3
                    «4»c + d
                «5»foo = 1
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "removes marker on broken expression in multi-line lambda" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x:
                    «2»c = x.+ 2
                    «3»d = x
                        «5».* 3
                    «4»c + d
                «5»foo = 1
                None
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = x:
                    «2»c = x.+ 2
                    «3»d = x
                        .* 3
                    «4»c + d
                «5»foo = 1
                None
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "works on multiple functions with unindented expression" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = 1
            «2»foo = 1
                «3»bar = 1
                «4»baz = 1
                None

            «5»def quux:
                «6»node = 1
                «7»n = node + 4
                n
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = 1
            «2»foo = 1
                «3»bar = 1
                «4»baz = 1
                None

            «5»def quux:
                «6»node = 1
                «7»n = node + 4
                n
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "works on multiple functions with overindented expression" $ let
        initialCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = 1
                    «2».+ 1
                «3»bar = 1
                «4»baz = 1
                None

            «5»def quux:
                «6»node = 1
                «7»n = node + 4
                n
            |]
        expectedCode = normalizeLunaCode [r|
            import Std.Base

            «0»def main:
                «1»node = 1
                    .+ 1
                «3»bar = 1
                «4»baz = 1
                None

            «5»def quux:
                «6»node = 1
                «7»n = node + 4
                n
            |]
        in sanitizeMarkers initialCode `shouldBe` expectedCode
    it "works on empty string"  $ sanitizeMarkers ""       `shouldBe` ""
    it "works on single marker" $ sanitizeMarkers "«2»"    `shouldBe` "«2»"
    it "works on double marker" $ sanitizeMarkers "«2»«3»" `shouldBe` "«2»"
    it "works on double marker with spaces" $
        sanitizeMarkers "«2»   «3»a" `shouldBe` "«2»   a"
