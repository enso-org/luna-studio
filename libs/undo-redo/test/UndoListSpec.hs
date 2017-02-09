{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module UndoListSpec (spec) where

import           Test.Hspec (around, describe, expectationFailure, it, shouldSatisfy, shouldBe, Spec)
import qualified Data.UUID.V4 as UUID
import           Data.ByteString.Lazy              (toStrict)
import           Data.Binary                       (encode, Binary)
import           Control.Error                   (runExceptT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State (evalStateT, forever)
import           Control.Error        (ExceptT, hoistEither)
import qualified Data.List                         as List

import qualified ZMQ.Bus.Data.Message              as Message
import           ZMQ.Bus.EndPoint                  (BusEndPoints (..))
import qualified ZMQ.Bus.EndPoint                as EP
import qualified ZMQ.Bus.Config       as Config
import qualified ZMQ.Bus.Trans                     as Bus
import qualified ZMQ.Bus.Bus                       as Bus

import qualified Data.Binary          as Binary
import           Data.ByteString.Lazy (fromStrict, toStrict)

import qualified Data.Text.Lazy                        as Text

import           Prologue

import qualified Empire.API.Topic                  as Topic
import           Empire.API.Graph.AddNode (Request (..), Result (..), NodeType (..))
import qualified Empire.API.Graph.AddNode as AddNode
import qualified Empire.API.Data.Node as Node
import qualified Empire.API.Data.Node (NodeType (..))

import           Empire.API.Data.GraphLocation (GraphLocation (..) )
import           Empire.API.Data.Breadcrumb (Breadcrumb (..))
import           Empire.API.Data.Library    (Library)
import qualified Empire.API.Response               as Response
import qualified Empire.API.Graph.Redo as Redo
import qualified Empire.API.Graph.Undo as Undo

import           Empire.API.Request as Request

import Undo (run', handleMessage, withBus, checkGuiId)
import UndoState (UndoState (..), Undo (..), UndoMessage (..))


generateGraphLocation ::  IO GraphLocation
generateGraphLocation = do
    uuid1 <- UUID.nextRandom
    return $ GraphLocation uuid1 0 $ Breadcrumb []

generateNode :: IO Node.Node
generateNode = do
    nodeId <- UUID.nextRandom
    return $ Node.Node nodeId "3" (Node.ExpressionNode "3") False def def Nothing

spec :: Spec
spec = describe "Undo-Redo for single user" $ do
    let state = UndoState [] [] []
    it "adds record to undo list when proper request is coming" $ do
        graphLocation <- generateGraphLocation
        reqID <- UUID.nextRandom
        guiID <- UUID.nextRandom
        node <- generateNode

        let response = Response.Response reqID (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node)
            topic = "empire.graph.node.add.response"
        (_, state1) <- run' state $ handleMessage $ Message.Message topic $ toStrict $ encode response
        case state1 of UndoState undo redo history -> do
                                                        undo `shouldSatisfy` ((== 1) . length)
                                                        redo `shouldSatisfy` ((== 0) . length)
                                                        history `shouldSatisfy` ((== 1) . length)
    it "compareId check" $ do
        graphLocation <- generateGraphLocation
        reqID <- UUID.nextRandom
        guiID <- UUID.nextRandom
        node <- generateNode

        let response = Response.Response reqID (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node)
            topic = "empire.graph.node.add.response"
        (_, state1) <- run' state $ handleMessage $ Message.Message topic $ toStrict $ encode response
        case state1 of UndoState undo redo history -> do
                                                        let msg = head undo
                                                        List.find (checkGuiId guiID) undo `shouldBe` (Just msg)

    it "undo request -> proper message is returned, undo list shorter by 1 and redo list longer" $ do
        graphLocation <- generateGraphLocation
        reqID1 <- UUID.nextRandom
        reqID2 <- UUID.nextRandom
        guiID <- UUID.nextRandom
        node <- generateNode

        let response = Response.Response reqID1 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node)
            topic = Topic.topic response

            undoReq = Request.Request reqID2 (Just guiID) (Undo.Request Undo.UndoRequest)
        (_, UndoState undo redo _) <- run' state $ do
            handleMessage $ Message.Message topic $ toStrict $ encode response
            handleMessage $ Message.Message "empire.undo.request" $ toStrict $ encode undoReq
        undo `shouldSatisfy` null
        redo `shouldSatisfy` ((== 1) . length)

    it "2 requests + undo" $ do
        graphLocation <- generateGraphLocation
        reqID1 <- UUID.nextRandom
        reqID2 <- UUID.nextRandom
        reqID3 <- UUID.nextRandom
        guiID <- UUID.nextRandom
        node1 <- generateNode
        node2 <- generateNode

        let response1 = Response.Response reqID1 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node1)
            response2 = Response.Response reqID2 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node2)
            topic = Topic.topic response1
            undoReq = Request.Request reqID3 (Just guiID) (Undo.Request Undo.UndoRequest)
        (_, UndoState undo redo history) <- run' state $ do
            handleMessage $ Message.Message topic $ toStrict $ encode response1
            handleMessage $ Message.Message topic $ toStrict $ encode response2
            handleMessage $ Message.Message "empire.undo.request" $ toStrict $ encode undoReq
        undo `shouldSatisfy` ((== 1) . length)
        redo `shouldSatisfy` ((== 1) . length)
        history `shouldSatisfy` ((== 3) . length)

    it "2 requests + 2 x undo" $ do
        graphLocation <- generateGraphLocation
        reqID1 <- UUID.nextRandom
        reqID2 <- UUID.nextRandom
        reqID3 <- UUID.nextRandom
        reqID4 <- UUID.nextRandom
        guiID <- UUID.nextRandom
        node1 <- generateNode
        node2 <- generateNode

        let response1 = Response.Response reqID1 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node1)
            response2 = Response.Response reqID2 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node2)
            topic = Topic.topic response1
            undoReq1 = Request.Request reqID3 (Just guiID) (Undo.Request Undo.UndoRequest)
            undoReq2 = Request.Request reqID4 (Just guiID) (Undo.Request Undo.UndoRequest)
        (_, UndoState undo redo history) <- run' state $ do
            handleMessage $ Message.Message topic $ toStrict $ encode response1
            handleMessage $ Message.Message topic $ toStrict $ encode response2
            handleMessage $ Message.Message "empire.undo.request" $ toStrict $ encode undoReq1
            handleMessage $ Message.Message "empire.undo.request" $ toStrict $ encode undoReq2
        undo `shouldSatisfy` null
        redo `shouldSatisfy` ((== 2) . length)
        history `shouldSatisfy` ((== 4) . length)

    it "2 requests + undo + redo" $ do
        graphLocation <- generateGraphLocation
        reqID1 <- UUID.nextRandom
        reqID2 <- UUID.nextRandom
        reqID3 <- UUID.nextRandom
        reqID4 <- UUID.nextRandom
        guiID <- UUID.nextRandom
        node1 <- generateNode
        node2 <- generateNode

        let response1 = Response.Response reqID1 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node1)
            response2 = Response.Response reqID2 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node2)
            topic = Topic.topic response1
            undoReq = Request.Request reqID3 (Just guiID) (Undo.Request Undo.UndoRequest)
            redoReq = Request.Request reqID4 (Just guiID) (Redo.Request Redo.RedoRequest)
        (_,  UndoState undo redo history) <- run' state $ do
            handleMessage $ Message.Message topic $ toStrict $ encode response1
            handleMessage $ Message.Message topic $ toStrict $ encode response2
            handleMessage $ Message.Message "empire.undo.request" $ toStrict $ encode undoReq
            handleMessage $ Message.Message "empire.redo.request" $ toStrict $ encode redoReq
        undo `shouldSatisfy` ((== 2) . length)
        redo `shouldSatisfy` null
        history `shouldSatisfy` ((== 4) . length)


    it "2 requests + undo + req" $ do
        graphLocation <- generateGraphLocation
        reqID1 <- UUID.nextRandom
        reqID2 <- UUID.nextRandom
        reqID3 <- UUID.nextRandom
        reqID4 <- UUID.nextRandom
        guiID <- UUID.nextRandom
        node1 <- generateNode
        node2 <- generateNode
        node3 <- generateNode

        let response1 = Response.Response reqID1 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node1)
            response2 = Response.Response reqID2 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node2)
            response3 = Response.Response reqID3 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node3)
            topic = Topic.topic response1
            undoReq = Request.Request reqID4 (Just guiID) (Undo.Request Undo.UndoRequest)
        (_, UndoState undo redo history) <- run' state $ do
            handleMessage $ Message.Message topic $ toStrict $ encode response1
            handleMessage $ Message.Message topic $ toStrict $ encode response2
            handleMessage $ Message.Message "empire.undo.request" $ toStrict $ encode undoReq
            handleMessage $ Message.Message topic $ toStrict $ encode response3
        undo `shouldSatisfy` ((== 2) . length)
        redo `shouldSatisfy` null
        history `shouldSatisfy` ((== 4) . length)


    it "2 requests + undo + not gui req" $ do
        graphLocation <- generateGraphLocation
        reqID1 <- UUID.nextRandom
        reqID2 <- UUID.nextRandom
        reqID3 <- UUID.nextRandom
        reqID4 <- UUID.nextRandom
        guiID <- UUID.nextRandom
        node1 <- generateNode
        node2 <- generateNode
        node3 <- generateNode

        let response1 = Response.Response reqID1 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node1)
            response2 = Response.Response reqID2 (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node2)
            response3 = Response.Response reqID3 (Nothing) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing) (Response.Ok ()) (Response.Ok node3)
            topic = Topic.topic response1
            undoReq = Request.Request reqID4 (Just guiID) (Undo.Request Undo.UndoRequest)
        (_, UndoState undo redo history) <- run' state $ do
            handleMessage $ Message.Message topic $ toStrict $ encode response1
            handleMessage $ Message.Message topic $ toStrict $ encode response2
            handleMessage $ Message.Message "empire.undo.request" $ toStrict $ encode undoReq
            handleMessage $ Message.Message topic $ toStrict $ encode response3
        undo `shouldSatisfy` ((== 1) . length)
        redo `shouldSatisfy` ((== 1) . length)
        history `shouldSatisfy` ((== 3) . length)
