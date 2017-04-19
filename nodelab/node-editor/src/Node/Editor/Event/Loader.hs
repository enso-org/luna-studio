module Node.Editor.Event.Loader where

import           JS.Config                     (getBackendAddress)
import           WebSocket                  (WebSocket)
import qualified WebSocket                  as WS
import           Luna.Prelude
import           Luna.Report


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
