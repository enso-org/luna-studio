module TextEditor.Event.Loader where

import           JS.Atom                 (pushStatus)
import           JS.Config               (getBackendAddress)
import           WebSocket            (WebSocket)
import qualified WebSocket            as WS
import           Common.Prelude


withActiveConnection :: (WebSocket -> IO ()) -> IO ()
withActiveConnection action = do
    (listenAddr, sendAddr) <- getBackendAddress
    socket <- WS.getWebSocket
    isOpen <- WS.isOpen socket
    let onConnectionClosed = putStrLn "ConnectionClosed."
    if isOpen then do
        action socket >> pushStatus (convert "Init") (convert "") (convert "")
    else do
        void $ WS.onOpen socket $ action socket >> pushStatus (convert "Init") (convert "") (convert "")
        void $ WS.onClose socket $ const onConnectionClosed
        void $ WS.onError socket onConnectionClosed
        void $ WS.connect socket listenAddr sendAddr
