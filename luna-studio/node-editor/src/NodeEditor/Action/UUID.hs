module NodeEditor.Action.UUID
    ( getUUID
    , registerRequest
    , unregisterRequest
    , isOwnRequest
    , module X
    ) where

import           Common.Action.Command    (Command)
import           Common.Prelude
import           Data.Map                 (member)
import           Data.Time.Clock          (getCurrentTime)
import           Data.UUID.Types          as X (UUID)
import           Data.UUID.Types.Internal (buildFromBytes)
import           NodeEditor.State.Global  (State, backend, nextRandom, pendingRequests)


getUUID :: Command State UUID
getUUID = do
    let nums = [1..16] :: [Integer]
    [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf] <- mapM (const nextRandom) nums
    return $ buildFromBytes 4 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf

registerRequest :: Command State UUID
registerRequest = do
    uuid <- getUUID
    time <- liftIO getCurrentTime
    backend . pendingRequests . at uuid ?= time
    return uuid

unregisterRequest :: UUID -> Command State ()
unregisterRequest uuid = backend . pendingRequests . at uuid .= Nothing

isOwnRequest :: UUID -> Command State Bool
isOwnRequest uuid = uses (backend . pendingRequests) $ member uuid
