module Luna.Studio.Event.Loader where

import           JS.Config                     (getBackendAddress)
import           JS.WebSocket                  (WebSocket)
import qualified JS.WebSocket                  as WS
import           Luna.Prelude
import           Luna.Studio.Report


withActiveConnection :: (WebSocket -> IO ()) -> IO ()
withActiveConnection action = do
    addr   <- getBackendAddress
    socket <- WS.getWebSocket
    isOpen <- WS.isOpen socket
    let onConnectionClosed = fatal "Connection closed."
    if isOpen then action socket
    else do
        void $ WS.onOpen  socket $ action socket
        void $ WS.onClose socket $ const onConnectionClosed
        void $ WS.onError socket onConnectionClosed
        void $ WS.connect socket addr
