{-# OPTIONS_GHC -fno-warn-orphans #-}
module TextEditor.State.Global where

import           Data.Aeson                           (ToJSON, toJSON)
import           Data.DateTime                        (DateTime)
import           Data.Set                             (Set)
import           Data.UUID.Types                      (UUID)
import           Data.Word                            (Word8)
import           LunaStudio.API.Graph.CollaborationUpdate (ClientId)
import           TextEditor.Action.Command           (Command)
import           TextEditor.Event.Event              (Event)
import           Common.Prelude
import           System.Random                        (StdGen)
import qualified System.Random                        as Random


data State = State { _lastEvent            :: Maybe Event
                   , _eventNum             :: Int
                   , _pendingRequests      :: Set UUID
                   , _lastEventTimestamp   :: DateTime
                   , _clientId             :: ClientId
                   , _random               :: StdGen
                   }

instance ToJSON StdGen where
    toJSON _ = toJSON "(random-generator)"

makeLenses ''State

mkState :: DateTime -> ClientId -> StdGen -> State
mkState = State def def def

nextRandom :: Command State Word8
nextRandom = uses random Random.random >>= \(val, rnd) -> random .= rnd >> return val
