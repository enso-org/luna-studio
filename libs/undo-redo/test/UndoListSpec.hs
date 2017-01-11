{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module UndoListSpec (spec) where

import           Test.Hspec (around, describe, expectationFailure, it, shouldSatisfy, Spec)
import qualified Data.UUID.V4 as UUID
import           Data.ByteString.Lazy              (toStrict)
import           Data.Binary                       (encode, Binary)
import           Control.Error                   (runExceptT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State (evalStateT, forever)
import           Control.Error        (ExceptT, hoistEither)
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
import           Empire.API.Graph.AddNode (Request (..), NodeType (..))
import qualified Empire.API.Graph.AddNode as AddNode
import           Empire.API.Data.GraphLocation (GraphLocation (..) )
import           Empire.API.Data.Breadcrumb (Breadcrumb (..))
import           Empire.API.Data.Library    (Library)


import           Empire.API.Request as Request

import Undo (run', handleMessage, UndoState (..), Undo (..), UndoMessage (..))


-- run :: BusEndPoints -> Undo () -> IO (Either Bus.Error (), UndoState)
-- run endPoints handle = do
--     Bus.runBus endPoints $ do
--         Bus.subscribe "empire."
--         let state = UndoState [] []
--         Bus.runBusT $ runReaderT (evalStateT (forever (runUndo handle)) state) endPoints
--
-- runWithState :: BusEndPoints -> UndoState -> IO (Either Bus.Error (), UndoState)
-- runWithState endPoints state = do
--     Bus.runBus endPoints $ do
--         Bus.subscribe "empire."
--         Bus.runBusT $ runReaderT (evalStateT (forever (runUndo receiveAndHandleMessage)) state) endPoints

generateGraphLocation ::  IO GraphLocation
generateGraphLocation = do
    uuid1 <- UUID.nextRandom
    return $ GraphLocation uuid1 0 $ Breadcrumb []

spec :: Spec
spec = describe "Undo-Redo" $ do
    it "adds record to undo list when proper request is coming" $ do
        graphLocation <- generateGraphLocation
        reqID <- UUID.nextRandom
        guiID <- UUID.nextRandom
        let request = Request.Request reqID (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing)
            topic = Topic.topic request
            mockEndpoints = BusEndPoints "tcp://127.0.0.1:30530" "tcp://127.0.0.1:30531" "tcp://127.0.0.1:30532"

        Right (_, state) <-run' mockEndpoints $ handleMessage $ Message.Message topic $ toStrict $ encode request
        case state of UndoState undo redo -> undo `shouldSatisfy` ((== 1) . length)

    -- it "undo request -> proper message is returned, undo list shorter by 1 and redo list longer" $ do
    --     graphLocation <- generateGraphLocation
    --     reqID <- UUID.nextRandom
    --     guiID <- UUID.nextRandom
    --     endPoints <- EP.clientFromConfig <$> Config.load
    --     let request = toStrict $ encode $ (Request.Request reqID (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing))
    --         topic = Topic.topic request
    --     handleMassage $ Message topic request
    --     -- undoReq <- doUndo $ Just guiID
