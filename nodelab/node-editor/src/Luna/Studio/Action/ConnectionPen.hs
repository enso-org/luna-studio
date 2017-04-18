{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.ConnectionPen
    ( startConnecting
    , startDisconnecting
    , connectMove
    , disconnectMove
    , stopConnecting
    , stopDisconnecting
    ) where

import           Luna.Studio.Action.ConnectionPen.ConnectionPen    (connectMove, startConnecting, stopConnecting)
import           Luna.Studio.Action.ConnectionPen.DisconnectionPen (disconnectMove, startDisconnecting, stopDisconnecting)
