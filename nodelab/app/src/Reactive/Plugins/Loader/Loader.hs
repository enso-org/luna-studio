module Reactive.Plugins.Loader.Loader where

import           Luna.Studio.Prelude
import qualified JS.WebSocket      as WS
import           JS.WebSocket      (WebSocket)
import           JS.Config         (getBackendAddress)
import           JS.UI             (displayConnectionClosedMessage)

withActiveConnection :: (WebSocket -> IO ()) -> IO ()
withActiveConnection action = do
    addr   <- getBackendAddress
    socket <- WS.getWebSocket
    void $ WS.onOpen  socket $ action socket
    void $ WS.onClose socket $ const displayConnectionClosedMessage
    void $ WS.onError socket displayConnectionClosedMessage
    void $ WS.connect socket addr
