{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module FileLoadSpec (spec) where

import           Data.List                     (find)
import qualified Data.Map                      as Map
import qualified Empire.API.Data.Graph         as Graph
import qualified Empire.API.Data.Node          as Node
import qualified Empire.API.Data.Port          as Port
import           Empire.API.Data.Breadcrumb    (Breadcrumb(..))
import           Empire.API.Data.GraphLocation (GraphLocation(..))
import           Empire.API.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           Empire.API.Data.TypeRep       (TypeRep(TStar))
import           Empire.ASTOp                  (runASTOp)
import qualified Empire.ASTOps.Parse           as ASTParse
import qualified Empire.ASTOps.Print           as ASTPrint
import qualified Empire.Commands.Graph         as Graph
import qualified Empire.Commands.GraphBuilder  as GraphBuilder
import qualified Empire.Commands.Library       as Library

import           Prologue                   hiding ((|>))

import           Test.Hspec (Spec, around, describe, it, xit, expectationFailure,
                             parallel, shouldBe, shouldMatchList, shouldStartWith)

import EmpireUtils

import           Text.RawString.QQ (r)


spec :: Spec
spec = around withChannels $ do
    describe "file loading" $ do
        it "parses unit" $ \env -> do
            let code = [r|pi ‹0›= 3.14
foo ‹1›= a: b: a + b
bar ‹2›= foo c 6
‹3›print pi
c ‹4›= 3
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                graph <- Graph.withGraph loc $ runASTOp $ GraphBuilder.buildGraph
                return graph
            withResult res $ \(Graph.Graph nodes connections _) -> do
                let Just pi = find (\node -> node ^. Node.name == "pi") nodes
                pi ^. Node.code `shouldBe` Just "3.14"
                pi ^. Node.canEnter `shouldBe` False
                let Just foo = find (\node -> node ^. Node.name == "foo") nodes
                foo ^. Node.code `shouldBe` Just "a: b: a + b"
                foo ^. Node.canEnter `shouldBe` True
                let Just bar = find (\node -> node ^. Node.name == "bar") nodes
                bar ^. Node.code `shouldBe` Just "foo c 6"
                bar ^. Node.canEnter `shouldBe` False
                let Just anon = find (\node -> node ^. Node.name == "") nodes
                anon ^. Node.code `shouldBe` Just "print pi"
                anon ^. Node.canEnter `shouldBe` False
                let Just c = find (\node -> node ^. Node.name == "c") nodes
                c ^. Node.code `shouldBe` Just "3"
                c ^. Node.canEnter `shouldBe` False
                connections `shouldMatchList` [
                      (outPortRef (pi ^. Node.nodeId) Port.All, inPortRef (anon ^. Node.nodeId) (Port.Arg 0))
                    , (outPortRef (c ^. Node.nodeId) Port.All, inPortRef (bar ^. Node.nodeId) (Port.Arg 0))
                    ]
