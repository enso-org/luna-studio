module Main where

import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as Char8
import qualified System.ZMQ4.Monadic   as ZMQ

import           Prelude               (putStrLn, print)
import           Prologue              hiding (putStrLn, print, liftIO, void)
import           Control.Monad.Trans   (liftIO)
import           ZMQ.Bus.Bus           (Bus)
import qualified ZMQ.Bus.Bus           as Bus
import qualified ZMQ.Bus.Data.Flag     as Flag
import qualified ZMQ.Bus.Data.Message  as Message
import           ZMQ.Bus.EndPoint      (BusEndPoints (BusEndPoints))



test :: Bus ()
test = void $ do
    Bus.subscribe ""
    clientID <- Bus.getClientID
    Bus.reply (Message.CorrelationID clientID 0) Flag.Enable
              (Message.Message "urm.undo.perform.request" (Char8.pack ""))
    liftIO $ putStrLn "sent"
    _ <- Bus.receive
    liftIO $ putStrLn "received"


endPoints :: BusEndPoints
endPoints = BusEndPoints "tcp://127.0.0.1:30530"
                         "tcp://127.0.0.1:30531"
                         "tcp://127.0.0.1:30532"


main :: IO ()
main = void $ do
    x <- ZMQ.runZMQ $ Bus.runBus endPoints test
    liftIO $ print x
