module JS.Clipboard
    ( registerCopyCallback
    , registerCutCallback
    , registerPasteCallback
    ) where

import           GHCJS.Foreign.Callback
import           GHCJS.Types
import           Luna.Studio.Prelude


foreign import javascript safe "window.addEventListener('copy', function(e){$1(e); e.preventDefault();})"
    registerCopyCallback' :: Callback (JSVal -> IO ()) -> IO ()

registerCopyCallback :: (JSVal -> IO ()) -> IO (IO ())
registerCopyCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerCopyCallback' wrappedCallback
    return $ return ()

foreign import javascript safe "window.addEventListener('cut', function(e){$1(e); e.preventDefault();})"
    registerCutCallback' :: Callback (JSVal -> IO ()) -> IO ()

registerCutCallback :: (JSVal -> IO ()) -> IO (IO ())
registerCutCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerCutCallback' wrappedCallback
    return $ return ()

foreign import javascript safe "window.addEventListener('paste', function(e){$1(e.clipboardData.getData('text/plain')); e.preventDefault();})"
    registerPasteCallback' :: Callback (JSVal -> IO ()) -> IO ()

registerPasteCallback :: (JSVal -> IO ()) -> IO (IO ())
registerPasteCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerPasteCallback' wrappedCallback
    return $ return ()
