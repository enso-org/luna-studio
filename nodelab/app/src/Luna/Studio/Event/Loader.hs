module Luna.Studio.Event.Loader where

import           JS.Config           (getBackendAddress)
import           JS.UI               (displayConnectionClosedMessage)
import           JS.WebSocket        (WebSocket)
import qualified JS.WebSocket        as WS
import           Luna.Studio.Prelude

withActiveConnection :: (WebSocket -> IO ()) -> IO ()
withActiveConnection action = do
    addr   <- getBackendAddress
    socket <- WS.getWebSocket
    void $ WS.onOpen  socket $ action socket
    void $ WS.onClose socket $ const displayConnectionClosedMessage
    void $ WS.onError socket displayConnectionClosedMessage
    void $ WS.connect socket addr
