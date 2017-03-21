module Luna.Studio.Event.Loader where

-- import           JS.Atom                 (pushNotification)
import           JS.Config               (getBackendAddress)
import           JS.WebSocket            (WebSocket)
import qualified JS.WebSocket            as WS
import           Luna.Studio.Error.Error
import           Luna.Studio.Prelude


withActiveConnection :: (WebSocket -> IO ()) -> IO ()
withActiveConnection action = do
    addr   <- getBackendAddress
    socket <- WS.getWebSocket
    isOpen <- WS.isOpen socket
    let onConnectionClosed = putStrLn "ConnectionClosed."
    if isOpen then action socket
    else do
        void $ WS.onOpen  socket $ action socket
        void $ WS.onClose socket $ const onConnectionClosed
        void $ WS.onError socket onConnectionClosed
        void $ WS.connect socket addr
