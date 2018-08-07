{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Control.Lens                ((^..))
import qualified Data.Map                    as Map
import           Empire.ASTOps.Parse         (SomeParserException)
import qualified Empire.Commands.Graph       as Graph
import           LunaStudio.Data.LabeledTree (LabeledTree (..))
import qualified LunaStudio.Data.Node        as Node
import qualified LunaStudio.Data.Port        as Port
import           LunaStudio.Data.PortDefault (PortDefault (Expression))
import           LunaStudio.Data.TypeRep     (TypeRep (TStar))

import           Prologue                    hiding ((|>))

import           Test.Hspec                  (Selector, Spec, around, describe, expectationFailure, it, parallel, shouldBe, shouldMatchList,
                                              shouldStartWith, shouldThrow, xit)

import           EmpireUtils


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "parser" $ do
        it "shows error on parse error" $ \env -> do
            u1 <- mkUUID
            let res = evalEmp env $ Graph.addNode top u1 ")()#%&&@^#&$....1foo0x3r2" def
            let parserException :: Selector SomeParserException
                parserException = const True
            res `shouldThrow` parserException
        it "parses 123" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ Graph.addNode top u1 "123" def
            withResult res $ \s' -> s' ^. Node.expression `shouldBe` "123"
        it "parses scientific notation with uppercase E" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ Graph.addNode top u1 "123E3" def
            withResult res $ \s' -> s' ^. Node.code `shouldBe` "123E3"
        it "parses \"foo\"" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ Graph.addNode top u1 "\"foo\"" def
            withResult res $ \s' -> s' ^. Node.expression `shouldBe` "\"foo\""
        it "parses constructors" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ Graph.addNode top u1 "Vector x y z" def
            withResult res $ \node -> do
                node  ^. Node.expression `shouldBe` "Vector x y z"
                (node ^. Node.outPorts)  `shouldBe`
                    LabeledTree def (Port.Port [] "vector1" TStar Port.NotConnected)
                (node ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "Vector x y z")
                    , Port.Port [Port.Arg 0] "x"    TStar (Port.WithDefault (Expression "x"))
                    , Port.Port [Port.Arg 1] "y"    TStar (Port.WithDefault (Expression "y"))
                    , Port.Port [Port.Arg 2] "z"    TStar (Port.WithDefault (Expression "z"))
                    ]
