{-# LANGUAGE RankNTypes #-}

module ValueDecoderSpec (spec) where

import           Empire.Prelude

import           Control.Monad       (foldM)
import           Data.Maybe          (isJust)
import           Empire.ASTOp        (ASTOp, runASTOp)
import           Empire.Commands.AST (valueDecoderForType)
import qualified Empire.Data.Graph   as Graph
import           Luna.IR.Function    as IR (arg)
import qualified Luna.IR             as IR
import           Text.Show.Functions ()

import           Test.Hspec (Expectation, Spec, describe, it,
                             shouldBe, shouldSatisfy)

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
