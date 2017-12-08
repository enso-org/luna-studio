{-# OPTIONS_GHC -fno-warn-orphans #-}
module TextEditor.State.Global where

import           Common.Action.Command                    (Command)
import           Common.Debug                             (HasRequestTimes, requestTimes)
import           Common.Prelude
import           Data.Aeson                               (ToJSON, toJSON)
import           Data.Map                                 (Map)
import           Data.Time.Clock                          (UTCTime)
import           Data.UUID.Types                          (UUID)
import           Data.Word                                (Word8)
import           LunaStudio.API.Graph.CollaborationUpdate (ClientId)
import           System.Random                            (StdGen)
import qualified System.Random                            as Random
import           TextEditor.Event.Event                   (Event)


data State = State { _lastEvent            :: Maybe Event
                   , _eventNum             :: Int
                   , _pendingRequests      :: Map UUID UTCTime
                   , _clientId             :: ClientId
                   , _random               :: StdGen
                   }

instance ToJSON StdGen where
    toJSON _ = toJSON "(random-generator)"

makeLenses ''State

mkState :: ClientId -> StdGen -> State
mkState = State def def def

nextRandom :: Command State Word8
nextRandom = uses random Random.random >>= \(val, rnd) -> random .= rnd >> return val

instance HasRequestTimes State where
    requestTimes = pendingRequests
