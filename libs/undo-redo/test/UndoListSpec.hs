{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module UndoListSpec (spec) where

import           Test.Hspec (around, describe, expectationFailure, it, shouldSatisfy, Spec)
import qualified Data.UUID.V4 as UUID
import           Data.ByteString.Lazy              (toStrict)
import           Data.Binary                       (encode, Binary)
import qualified ZMQ.Bus.Env                     as Env
import qualified System.ZMQ4             as ZMQ
import qualified ZMQ.Bus.EndPoint                as EP
import qualified ZMQ.Bus.Data.Topic              as ZMQTopic
import qualified ZMQ.Bus.Control.Handler.Methods as Methods
import           ZMQ.Bus.EndPoint                  (BusEndPoints)
import qualified ZMQ.Bus.Config       as Config
import           Control.Error                   (runExceptT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State (execStateT)
import           Control.Error        (ExceptT, hoistEither)
import qualified ZMQ.RPC.Response     as Response
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

import Undo (collectedMessage, UndoState (..), Undo (..), UndoMessage (..))

generateGraphLocation ::  IO GraphLocation
generateGraphLocation = do
    uuid1 <- UUID.nextRandom
    return $ GraphLocation uuid1 0 $ Breadcrumb []

type Serializable a b = (Binary a, Binary b, Show b)

processResponse :: _ result -> Either String result
processResponse (Response.Result _ result) = return result
processResponse (Response.Exception _ msg) = Left msg

query :: (ZMQ.Sender t, ZMQ.Receiver t, Serializable request result)
      => ZMQ.Socket t
      -> request
      -> IO (Either String result)
query socket request = do
    response <- queryRaw socket request
    return $ processResponse response

queryRaw :: (ZMQ.Sender t, ZMQ.Receiver t, Serializable request result)
          => ZMQ.Socket t -> request -> IO (_ result)
queryRaw socket request = do
    ZMQ.send socket [] $ toStrict $ Binary.encode request
    encoded_response <- ZMQ.receive socket
    case Binary.decodeOrFail $ fromStrict encoded_response of
        Right r -> r ^. _3
        Left  l -> l ^. _3

runAndReturnUndoList :: BusEndPoints -> Undo z () -> IO ([UndoMessage])
runAndReturnUndoList endPoints act = ZMQ.withContext $ \context -> do
    ZMQ.withSocket context $ \socket -> do
        ZMQ.withSocket context $ \subSocket ->do
            ZMQ.withSocket context $ \pushSocket ->do
                clientID <- do
                    ZMQ.connect socket $ EP.controlEndPoint endPoints
                    let request = Methods.CreateID
                    Right response <- query socket request
                    ZMQ.close socket
                    return $ Methods.clientID response
                ZMQ.subscribe subSocket $ ZMQTopic.toByteString "empire." -- FIXME [WD]: nie uzywamy literalow, nigdy --- data EmpireTopic i uzywac go zamiast stringa jakos ?
                ZMQ.connect subSocket  $ EP.pubEndPoint  endPoints
                ZMQ.connect pushSocket $ EP.pullEndPoint endPoints
                let emptyState = UndoState [] [] (Env.BusEnv subSocket pushSocket clientID 0)
                    Undo foo = act
                UndoState u _ _ <- flip runReaderT endPoints $ execStateT foo emptyState
                return u

spec :: Spec
spec = describe "Undo-Redo" $ do
    it "adds record to undo list when proper request is coming" $ do
        graphLocation <- generateGraphLocation
        reqID <- UUID.nextRandom
        guiID <- UUID.nextRandom
        endPoints <- EP.clientFromConfig <$> Config.load
        let request = toStrict $ encode $ (Request.Request reqID (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing))
            topic = Topic.topic request
        state <- runAndReturnUndoList endPoints $ collectedMessage topic request
        state `shouldSatisfy` ((== 1) . length)

    it "undo request -> proper message is returned, undo list shorter by 1 and redo list longer" $ do
        graphLocation <- generateGraphLocation
        reqID <- UUID.nextRandom
        guiID <- UUID.nextRandom
        endPoints <- EP.clientFromConfig <$> Config.load
        let request = toStrict $ encode $ (Request.Request reqID (Just guiID) (AddNode.Request graphLocation (ExpressionNode "3") def Nothing Nothing))
            topic = Topic.topic request
        run endPoints $ collectedMessage topic request
        -- undoReq <- doUndo $ Just guiID
