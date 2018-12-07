module Test.Undo.InverseSpec where

import Empire.Prelude

import qualified Data.Binary                        as Binary
import qualified Data.Map                           as Map
import qualified Empire.Commands.Graph              as Graph
import qualified LunaStudio.API.Graph.AddConnection as AddConnection
import qualified LunaStudio.API.Graph.AddPort       as AddPort
import qualified LunaStudio.API.Graph.SetNodesMeta  as SetNodesMeta
import qualified LunaStudio.API.Graph.Transaction   as Transaction
import qualified LunaStudio.API.Topic               as Topic
import qualified LunaStudio.Data.Graph              as Graph
import qualified LunaStudio.Data.Node               as Node
import qualified LunaStudio.Data.NodeMeta           as NodeMeta
import qualified LunaStudio.Data.Position           as Position

import Empire.ApiHandlers             (perform, buildInverse)
import LunaStudio.Data.NodeMeta       (NodeMeta(NodeMeta))
import LunaStudio.Data.Port           (InPortIndex (Arg, Self),
                                       OutPortIndex(Projection))
import LunaStudio.Data.PortRef        (AnyPortRef (InPortRef'), InPortRef(..),
                                       OutPortRef(..))
import Test.Hspec                     (Spec, it)
import Test.Hspec.Empire              (findNodeIdByName,
                                       runTests, testCase)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Text.RawString.QQ              (r)


spec :: Spec
spec = runTests "undo" $ do
    it "should undo dropping node on connection" $ let
        code = [r|
            def main:
                number1 = 3
                negate1 = negate
                expr1 = number1 +
                None
            |]
        in testCase code code $ \gl -> do
            graph        <- Graph.getGraph gl
            Just number1 <- findNodeIdByName gl "number1"
            Just expr1   <- findNodeIdByName gl "expr1"
            Just negate1 <- findNodeIdByName gl "negate1"
            connCenter <- do
                metas <- Graph.getNodeMetas gl $ map convert [number1, expr1]
                let [numberPos] = [ pos |
                        Just (nid, (view NodeMeta.position -> pos)) <- metas,
                        nid == convert number1 ]
                let [exprPos] = [ pos |
                        Just (nid, (view NodeMeta.position -> pos)) <- metas,
                        nid == convert expr1 ]
                return $ Position.averagePosition numberPos exprPos
            let connLeftReq =
                    AddConnection.Request
                        gl
                        (Left $ OutPortRef (convert number1) [])
                        (Left $ InPortRef' $ InPortRef (convert negate1) [Self])
                connRightReq =
                    AddConnection.Request
                        gl
                        (Left $ OutPortRef (convert negate1) [])
                        (Left $ InPortRef' $ InPortRef (convert expr1) [Arg 0])
                moveNodeReq =
                    SetNodesMeta.Request
                        gl
                        (Map.singleton negate1 (NodeMeta connCenter False Nothing))
                mkReq req = (Topic.topic' req, Binary.encode req)
                transaction = Transaction.Request gl [
                        mkReq connLeftReq
                      , mkReq connRightReq
                      , mkReq moveNodeReq
                      ]
            inv <- buildInverse transaction
            perform transaction
            perform inv
            graph' <- Graph.getGraph gl
            graph' `shouldBe` graph
    it "should undo adding port with connection" $ let
        code = [r|
            def main:
                negate1 = negate
                None
            |]
        in testCase code code $ \gl -> do
            graph        <- Graph.getGraph gl
            Just negate1 <- findNodeIdByName gl "negate1"
            let Just inputSidebar =
                   convert (graph ^? Graph.inputSidebar . _Just . Node.nodeId)
                addPortReq =
                    AddPort.Request
                        gl
                        (OutPortRef inputSidebar [Projection 0])
                        [InPortRef' $ InPortRef (convert negate1) [Self]]
                        (Just "a")
            inv <- buildInverse addPortReq
            perform addPortReq
            perform inv
            graph' <- Graph.getGraph gl
            graph' `shouldBe` graph

