module TextEditor.Action.UUID
    ( getUUID
    , registerRequest
    , unregisterRequest
    , isOwnRequest
    ) where

import           Common.Prelude
import           Data.UUID.Types           (UUID)
import           Data.UUID.Types.Internal  (buildFromBytes)
import           TextEditor.Action.Command (Command)
import           TextEditor.State.Global   (State, nextRandom, pendingRequests)

import qualified Data.Map                  as Map
import           Data.Time.Clock           (UTCTime, getCurrentTime)

getUUID :: Command State UUID
getUUID = do
    let nums = [1..16] :: [Integer]
    [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf] <- mapM (const nextRandom) nums
    return $ buildFromBytes 4 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf

registerRequest :: Command State UUID
registerRequest = do
    uuid <- getUUID
    time <- liftIO getCurrentTime
    pendingRequests %= Map.insert uuid time
    return uuid

unregisterRequest :: UUID -> Command State ()
unregisterRequest uuid = pendingRequests %= Map.delete uuid

isOwnRequest :: UUID -> Command State Bool
isOwnRequest uuid = uses pendingRequests $ Map.member uuid
