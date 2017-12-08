
{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( activeLocation
    , insertCode
    , pushInterpreterUpdate
    , pushStatus
    , setBuffer
    , setClipboard
    , subscribeDiffs
    , subscribeEventListenerInternal
    ) where

import           Common.Data.JSON              (fromJSONVal, toJSONVal)
import           Common.Prelude                hiding (toList)
import           Control.Monad.Trans.Maybe     (MaybeT (MaybeT), runMaybeT)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure            (PToJSVal (pToJSVal))
import           LunaStudio.Data.Diff          (Diff)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Point         (Point)
import           TextEditor.Event.Internal     (InternalEvent, InternalEvent (..))
import           TextEditor.Event.Text         (TextEvent (TextEvent))
import qualified TextEditor.Event.Text         as TextEvent


foreign import javascript safe "atomCallbackTextEditor.insertCode($1, $2)"
    insertCode' :: JSString -> JSVal -> IO ()

foreign import javascript safe "atomCallbackTextEditor.setBuffer($1, $2)"
    setBuffer :: JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.setClipboard($1, $2)"
    setClipboard :: JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.pushStatus($1, $2, $3)"
    pushStatus :: JSString -> JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.pushInterpreterUpdate($1, $2)"
    pushInterpreterUpdate' :: JSString -> JSVal -> IO ()

foreign import javascript safe "atomCallbackTextEditor.subscribeEventListenerInternal($1)"
    subscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeEventListenerInternal()"
    unsubscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.subscribeDiffs($1)"
    subscribeDiffs' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeDiffs()"
    unsubscribeDiffs' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "globalRegistry.activeLocation"
    activeLocation' :: IO JSVal


instance ToJSVal Point where toJSVal = toJSONVal
instance ToJSVal Diff  where toJSVal = toJSONVal
instance FromJSVal Diff where fromJSVal = fromJSONVal

instance FromJSVal GraphLocation where fromJSVal = fromJSONVal
instance FromJSVal InternalEvent where fromJSVal = fromJSONVal

instance FromJSVal TextEvent where
    fromJSVal jsval = runMaybeT $ do
        location <- MaybeT activeLocation
        diffs    <- MaybeT $ fromJSVal jsval
        return $ TextEvent location diffs

activeLocation :: MonadIO m => m (Maybe GraphLocation)
activeLocation = liftIO $ fromJSVal =<< activeLocation'

subscribeDiffs :: (TextEvent -> IO ()) -> IO (IO ())
subscribeDiffs callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ \js -> withJustM_ (fromJSVal js) callback
    subscribeDiffs' wrappedCallback
    return $ unsubscribeDiffs' wrappedCallback >> releaseCallback wrappedCallback

insertCode :: TextEvent -> IO ()
insertCode textEvent = do
    let uri   = textEvent ^. TextEvent.filePath . to convert
        diffs = textEvent ^. TextEvent.diffs
    insertCode' uri =<< toJSVal diffs

subscribeEventListenerInternal :: (InternalEvent -> IO ()) -> IO (IO ())
subscribeEventListenerInternal callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ \jsval -> withJustM (fromJSVal jsval) callback
    subscribeEventListenerInternal' wrappedCallback
    return $ unsubscribeEventListenerInternal' wrappedCallback >> releaseCallback wrappedCallback

pushInterpreterUpdate :: MonadIO m => String -> Maybe Text -> m ()
pushInterpreterUpdate name value = liftIO $ pushInterpreterUpdate' (convert name) (pToJSVal value)
