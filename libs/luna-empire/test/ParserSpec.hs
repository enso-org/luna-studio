{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified Data.Map                     as Map
import           Empire.API.Data.PortDefault (PortDefault(Expression))
import qualified Empire.API.Data.Node         as Node
import qualified Empire.API.Data.Port         as Port
import           Empire.API.Data.LabeledTree  (LabeledTree (..))
import           Empire.API.Data.TypeRep      (TypeRep(TStar))
import qualified Empire.Commands.Graph        as Graph

import           Prologue                   hiding ((|>))

import           Test.Hspec (Spec, around, describe, it, xit, expectationFailure,
                             shouldBe, shouldMatchList, shouldStartWith)

import EmpireUtils


spec :: Spec
spec = around withChannels $ do
    describe "parser" $ do
        it "shows error on parse error" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ Graph.addNode top u1 ")()#%&&@^#&$....1foo0x3r2" def
            case res of
                Right _ -> expectationFailure "should throw exception"
                Left err -> err `shouldStartWith` "ParserException"
        it "parses 123" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ Graph.addNode top u1 "123" def
            withResult res $ \s' -> s' ^. Node.expression `shouldBe` "123"
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
                    LabeledTree def (Port.Port (Port.OutPortId []) "node1" TStar Port.NotConnected)
                (node ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port (Port.InPortId [])           "base" TStar (Port.WithDefault $ Expression "Vector x y z")
                    , Port.Port (Port.InPortId [Port.Arg 0]) "x"    TStar (Port.WithDefault (Expression "x"))
                    , Port.Port (Port.InPortId [Port.Arg 1]) "y"    TStar (Port.WithDefault (Expression "y"))
                    , Port.Port (Port.InPortId [Port.Arg 2]) "z"    TStar (Port.WithDefault (Expression "z"))
                    ]
