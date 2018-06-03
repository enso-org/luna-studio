module NodeEditor.Event.Loader where

import           JS.Config                  (getBackendAddress)
import           Common.Prelude
import           Common.Report              (fatal)
import           WebSocket                  (WebSocket)
import qualified WebSocket                  as WS


withActiveConnection :: (WebSocket -> IO ()) -> IO ()
withActiveConnection action = do
    (listenAddr, sendAddr) <- getBackendAddress
    socket <- WS.getWebSocket
    isOpen <- WS.isOpen socket
    let onConnectionClosed = fatal "Connection closed."
    if isOpen then action socket
    else do
        void $ WS.onOpen  socket $ action socket
        void $ WS.onClose socket $ const onConnectionClosed
        void $ WS.onError socket onConnectionClosed
        void $ WS.connect socket listenAddr sendAddr
