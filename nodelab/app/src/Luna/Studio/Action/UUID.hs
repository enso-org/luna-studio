module Luna.Studio.Action.UUID
    ( getUUID
    , registerRequest
    , unregisterRequest
    , isOwnRequest
    ) where

import qualified Data.Set                   as Set
import           Data.UUID.Types            (UUID)
import           Data.UUID.Types.Internal   (buildFromBytes)
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global   (State, nextRandom, pendingRequests)

getUUID :: Command State UUID
getUUID = do
    let nums = [1..16] :: [Integer]
    [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf] <- mapM (const nextRandom) nums
    return $ buildFromBytes 4 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf

registerRequest :: Command State UUID
registerRequest = do
    uuid <- getUUID
    pendingRequests %= Set.insert uuid
    return uuid

unregisterRequest :: UUID -> Command State ()
unregisterRequest uuid = pendingRequests %= Set.delete uuid

isOwnRequest :: UUID -> Command State Bool
isOwnRequest uuid = uses pendingRequests $ Set.member uuid
