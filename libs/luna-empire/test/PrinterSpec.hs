{-# LANGUAGE OverloadedStrings #-}

module PrinterSpec (spec) where

import qualified Empire.Commands.Graph      as Graph
import qualified Empire.Commands.GraphUtils as GraphUtils
import           Empire.ASTOp               (runASTOp)
import           Empire.ASTOps.Print        (printNodeExpression)
import           Empire.Data.Graph          (ast)

import           Prologue                   hiding ((|>))

import           Test.Hspec (Spec, around, describe, it,
                             shouldBe, shouldMatchList)

import EmpireUtils


spec :: Spec
spec = around withChannels $ do
    describe "pretty-printer" $ do
        it "ignores nodes outside lambda while pretty-printing code inside it" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            u5 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                Graph.addNode top u2 "3" def
                Graph.addNode top u3 "4" def
                Graph.addNode loc' u4 "5" def
                Graph.addNode loc' u5 "6" def
                (,) <$> Graph.getCode top <*> Graph.getCode loc'
            withResult res $ \(topCode, lambdaCode) -> do
                lines topCode `shouldMatchList` ["node3 = 4", "node2 = 3", "foo = -> $in0 in0"]
                lines lambdaCode `shouldMatchList` ["def foo in0:", "    node5 = 6", "    node4 = 5", "    in0"]
        it "properly pretty-prints functions" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "-> $a $b a + b" def
                Graph.getCode (top |> u1)
            withResult res $ \pprCode -> do
                lines pprCode `shouldMatchList` ["def node1 a b:", "    a + b"]
        it "prints `def foo` function" $ \env -> do
          u1 <- mkUUID
          res <- evalEmp env $ do
              Graph.addNode top u1 "def foo" def
              Graph.getCode (top |> u1)
          withResult res $ \pprCode -> do
              lines pprCode `shouldMatchList` ["def foo in0:", "    in0"]
        it "prints node name for `def foo`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
                Graph.withGraph top $ do
                    target <- GraphUtils.getASTTarget u1
                    zoom ast $ runASTOp $ printNodeExpression target
            withResult res $ \expr -> expr `shouldBe` "-> $in0 in0"
