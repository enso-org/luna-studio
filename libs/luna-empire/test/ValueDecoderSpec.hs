{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}

module ValueDecoderSpec (spec) where

import           Empire.Prelude

import           Control.Monad       (foldM)
import           Data.Maybe          (isJust)
import           Empire.API.Data.DefaultValue (Value(..))
import           Empire.ASTOp        (ASTOp, runASTOp)
import           Empire.Commands.AST (valueDecoderForType)
import qualified Empire.Data.Graph   as Graph
import           Luna.IR.Function    as IR (arg)
import qualified Luna.IR             as IR
import qualified Luna.Pass.Evaluation.Interpreter.Value as Luna
import           Text.Show.Functions ()
import           Test.Hspec (Expectation, Spec, describe, it,
                             shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck.Monadic (assert, monadicIO)
import           Test.QuickCheck.Property (Property)

import           EmpireUtils

runGraph :: (forall m. ASTOp m => m a) -> IO (Either String a)
runGraph act = do
    g <- Graph.defaultGraph
    -- undefined is ok, we won't ever use CommunicationEnv
    res <- runEmpire undefined g $ runASTOp act
    return $ fst res

existsDecoder :: (forall m. ASTOp m => m (IR.Expr l)) -> Expectation
existsDecoder act = do
    res <- runGraph $ do
        v <- IR.generalize <$> act
        valueDecoderForType v
    withResult res $ \m -> m `shouldSatisfy` isJust

data DecoderNotFoundException = DecoderNotFoundException [String]
    deriving Show

instance Exception DecoderNotFoundException

data DecodingException = DecodingException
    deriving (Show, Exception)

roundtrips :: Luna.ToData a => [String] -> a -> IO Value
roundtrips types value = do
    res <- runGraph $ do
        rep <- conses types
        decoder <- valueDecoderForType rep
        case decoder of
            Nothing       -> throwM $ DecoderNotFoundException types
            Just decoder' -> do
                let data' = Luna.unsafeToData value
                    decoded = decoder' data'
                return decoded
    case res of
        Left _err -> throwM DecodingException
        Right v  -> return v

check :: Luna.ToData a => [String] -> (a -> Value) -> a -> Property
check types con v = monadicIO $ do
    ret <- liftIO $ roundtrips types v
    assert $ ret == con v

check' :: Luna.ToData a
        => [String] -> (f (g String) -> Value) -> (a -> (f (g String))) -> a -> Property
check' types con f v = monadicIO $ do
    ret <- liftIO $ roundtrips types v
    assert $ ret == con (f v)

cons :: ASTOp m => String -> m IR.AnyExpr
cons name = do
    name' <- IR.string name
    IR.generalize <$> IR.cons name'

conses :: ASTOp m => [String] -> m IR.AnyExpr
conses names = do
    let (seed:rest) = reverse names
    seedM <- cons seed
    foldM (\acc new -> do
        newName <- cons new
        IR.generalize <$> IR.app newName (IR.arg acc)) seedM rest

spec :: Spec
spec = do
    describe "valueDecoderForType" $ do
        it "Int"               $ existsDecoder $ cons "Int"
        it "String"            $ existsDecoder $ cons "String"
        it "Double"            $ existsDecoder $ cons "Double"
        it "Bool"              $ existsDecoder $ cons "Bool"
        it "Histogram"         $ existsDecoder $ cons "Histogram"
        it "IntPairList"       $ existsDecoder $ cons "IntPairList"
        it "DoublePairList"    $ existsDecoder $ cons "DoublePairList"
        it "RGBColor"          $ existsDecoder $ cons "RGBColor"
        it "Stream Int"        $ existsDecoder $ conses ["Stream", "Int"]
        it "List Int"          $ existsDecoder $ conses ["List", "Int"]
        it "List Double"       $ existsDecoder $ conses ["List", "Double"]
        it "List Bool"         $ existsDecoder $ conses ["List", "Bool"]
        it "List String"       $ existsDecoder $ conses ["List", "String"]
        it "List Maybe Int"    $ existsDecoder $ conses ["List", "Maybe", "Int"]
        it "List Maybe Double" $ existsDecoder $ conses ["List", "Maybe", "Double"]
        it "List Maybe Bool"   $ existsDecoder $ conses ["List", "Maybe", "Bool"]
        it "List Maybe String" $ existsDecoder $ conses ["List", "Maybe", "String"]
        it "Maybe Int"         $ existsDecoder $ conses ["Maybe", "Int"]
        it "Maybe Double"      $ existsDecoder $ conses ["Maybe", "Double"]
        it "Maybe Bool"        $ existsDecoder $ conses ["Maybe", "Bool"]
        it "Maybe String"      $ existsDecoder $ conses ["Maybe", "String"]
        it "Map String Int"    $ existsDecoder $ conses ["Map", "String", "Int"]
        it "Map String Double" $ existsDecoder $ conses ["Map", "String", "Double"]
        it "Map String Bool"   $ existsDecoder $ conses ["Map", "String", "Bool"]
        it "Map String String" $ existsDecoder $ conses ["Map", "String", "String"]
    describe "round-trips" $ do
        prop "Int"            $ check ["Int"]             IntValue
        prop "String"         $ check ["String"]          StringValue
        prop "Double"         $ check ["Double"]          DoubleValue
        prop "Bool"           $ check ["Bool"]            BoolValue
        prop "Histogram"      $ check ["Histogram"]       Histogram
        prop "IntPairList"    $ check ["IntPairList"]     IntPairList
        prop "DoublePairList" $ check ["DoublePairList"]  DoublePairList
        prop "Maybe Int"      $ check ["Maybe", "Int"]    IntMaybe
        prop "Maybe Double"   $ check ["Maybe", "Double"] DoubleMaybe
        prop "Maybe Bool"     $ check ["Maybe", "Bool"]   BoolMaybe
        prop "Maybe String"   $ check ["Maybe", "String"] StringMaybe
        prop "List Int"       $ check ["List", "Int"]     IntList
        prop "List Double"    $ check ["List", "Double"]  DoubleList
        prop "List Bool"      $ check ["List", "Bool"]    BoolList
        prop "List String"    $ check ["List", "String"]  StringList
        prop "List Maybe Int" $
            check' ["List", "Maybe", "Int"]    StringMaybeList (fmap (fmap show) :: [Maybe Int] -> [Maybe String])
        prop "List Maybe Double" $
            check' ["List", "Maybe", "Double"] StringMaybeList (fmap (fmap show) :: [Maybe Double] -> [Maybe String])
        prop "List Maybe Bool" $
            check' ["List", "Maybe", "Bool"]   StringMaybeList (fmap (fmap show) :: [Maybe Bool] -> [Maybe String])
        prop "List Maybe String" $
            check' ["List", "Maybe", "String"] StringMaybeList (id :: [Maybe String] -> [Maybe String])
        prop "Map String Int" $
            check' ["Map", "String", "Int"]    StringStringMap (fmap (fmap show) :: [(String, Int)] -> [(String, String)])
        prop "Map String Double" $
            check' ["Map", "String", "Double"] StringStringMap (fmap (fmap show) :: [(String, Double)] -> [(String, String)])
        prop "Map String Bool" $
            check' ["Map", "String", "Bool"]   StringStringMap (fmap (fmap show) :: [(String, Bool)] -> [(String, String)])
        prop "Map String String" $
            check' ["Map", "String", "String"] StringStringMap (id :: [(String, String)] -> [(String, String)])
