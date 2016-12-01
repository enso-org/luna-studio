{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Undo where

import Control.Monad.State
import Control.Lens
import Control.Exception.Base
import           Data.ByteString                   (ByteString)
import Data.Binary (Binary)
import Data.Maybe
import Prelude
import qualified ZMQ.Bus.Bus                       as Bus
import qualified ZMQ.Bus.Data.Message              as Message
import           ZMQ.Bus.Data.MessageFrame         (MessageFrame (MessageFrame))
import           ZMQ.Bus.EndPoint                  (BusEndPoints)
import qualified ZMQ.Bus.Trans                     as Bus



data UndoItem =  UndoItem { _userId  :: String
                          , _content :: ByteString
                          } deriving (Show, Eq)

data UndoMessage = UndoMessage { _topic :: String
                               , _message :: ByteString
                               } deriving (Show, Eq)

data UndoList = UndoList { _undo :: [UndoItem]
                         , _redo :: [UndoItem]
                         } deriving (Show, Eq)


type Undo a = forall m. (MonadIO m, MonadState UndoList m) =>  m a

makeLenses ''UndoList
makeLenses ''UndoItem
makeLenses ''UndoMessage

empty :: UndoList
empty = UndoList [] []

runUndo :: BusEndPoints -> IO ()
runUndo endPoints = evalStateT (collectEvents endPoints) empty

undoTopic = "undo.undo"
redoTopic = "undo.redo"
dummyMsg :: ByteString
dummyMsg = "dummy"

collectEvents :: BusEndPoints -> Undo ()
collectEvents endPoints = do
    msgFrame <- Bus.runBus endPoints $ Bus.receive'
    case (join msgFrame) of
        Right (MessageFrame msg corId senderId lastFrm) -> do
            let topic = msg ^. Message.topic
                userId = show senderId
                content = msg ^. Message.message
            collectedMessage userId content

collectedMessage :: String -> ByteString -> Undo ()
collectedMessage userId content = items %= (UndoItem userId content :)
