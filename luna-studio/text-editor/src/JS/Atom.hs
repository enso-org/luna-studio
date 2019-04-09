
{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( activeLocation
    , insertCode
    , OpenedFile(OpenedFile)
    , openedFiles
    , openedFileUri
    , openedFileContents
    , pushInterpreterUpdate
    , pushStatus
    , setBuffer
    , setClipboard
    , setFileBuffer
    , subscribeDiffs
    , subscribeEventListenerInternal
    ) where

import           Common.Data.JSON              (fromJSONVal, toJSONVal)
import           Common.Prelude                hiding (toList)
import           Control.Monad.Trans.Maybe     (MaybeT (MaybeT), runMaybeT)
import           Data.Aeson.Types              (FromJSON)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure            (PToJSVal (pToJSVal))
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Point         (Point)
import           LunaStudio.Data.TextDiff      (TextDiff)
import           TextEditor.Event.Internal     (InternalEvent, InternalEvent (..))
import           TextEditor.Event.Text         (TextEvent (TextEvent))
import qualified TextEditor.Event.Text         as TextEvent


foreign import javascript safe "atomCallbackTextEditor.insertCode($1, $2)"
    jsInsertCode :: JSString -> JSVal -> IO ()

foreign import javascript safe "atomCallbackTextEditor.setBuffer($1, $2)"
    jsSetBuffer :: JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.setClipboard($1, $2)"
    jsSetClipboard :: JSString -> JSString -> IO ()

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

foreign import javascript safe "res = Array.from(new Set(atom.workspace.getPaneItems().filter(function (a) { return Object.getPrototypeOf(a).constructor.name.startsWith(\"LunaCodeEditor\"); }))); $r = res.map(function (a) { return {_openedFileUri: a.uri, _openedFileContents: a.getText() }; })"
    openedFiles' :: IO JSVal

foreign import javascript safe "atom.workspace.getPaneItems().filter(function (a) { return Object.getPrototypeOf(a).constructor.name.startsWith(\"LunaCodeEditor\") && a.uri === $1; }).map(function (a) { a.setText($2); })"
    setFileBuffer' :: JSString -> JSVal -> IO ()


instance ToJSVal Point where toJSVal = toJSONVal
instance ToJSVal   TextDiff where toJSVal = toJSONVal
instance FromJSVal TextDiff where fromJSVal = fromJSONVal

instance FromJSVal GraphLocation where fromJSVal = fromJSONVal
instance FromJSVal InternalEvent where fromJSVal = fromJSONVal

data OpenedFile = OpenedFile
    { _openedFileUri      :: String
    , _openedFileContents :: Text
    } deriving (Show, Generic)

makeLenses ''OpenedFile

instance FromJSON OpenedFile
instance FromJSVal OpenedFile where fromJSVal = fromJSONVal

instance FromJSVal TextEvent where
    fromJSVal jsval = runMaybeT $ do
        location <- MaybeT activeLocation
        diffs    <- MaybeT $ fromJSVal jsval
        return $ TextEvent location diffs

activeLocation :: MonadIO m => m (Maybe GraphLocation)
activeLocation = liftIO $ fromJSVal =<< activeLocation'

openedFiles :: MonadIO m => m (Maybe [OpenedFile])
openedFiles = liftIO $ fromJSVal =<< openedFiles'

setFileBuffer :: MonadIO m => String -> Text -> m ()
setFileBuffer file code = liftIO $ setFileBuffer' (convert file) (pToJSVal code)

subscribeDiffs :: (TextEvent -> IO ()) -> IO (IO ())
subscribeDiffs callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ \js -> withJustM_ (fromJSVal js) callback
    subscribeDiffs' wrappedCallback
    return $ unsubscribeDiffs' wrappedCallback >> releaseCallback wrappedCallback

insertCode :: MonadIO m => TextEvent -> m ()
insertCode textEvent = liftIO $ do
    let uri   = textEvent ^. TextEvent.filePath . to convert
        diffs = textEvent ^. TextEvent.diffs
    jsInsertCode uri =<< toJSVal diffs

setBuffer :: MonadIO m => Text -> Text -> m ()
setBuffer uri code = liftIO $ jsSetBuffer (convert uri) (convert code)

setClipboard :: MonadIO m => Text -> Text -> m ()
setClipboard uri code = liftIO $ jsSetClipboard (convert uri) (convert uri)

subscribeEventListenerInternal :: (InternalEvent -> IO ()) -> IO (IO ())
subscribeEventListenerInternal callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ \jsval -> withJustM (fromJSVal jsval) callback
    subscribeEventListenerInternal' wrappedCallback
    return $ unsubscribeEventListenerInternal' wrappedCallback >> releaseCallback wrappedCallback

pushInterpreterUpdate :: MonadIO m => String -> Maybe Text -> m ()
pushInterpreterUpdate name value = liftIO $ pushInterpreterUpdate' (convert name) (pToJSVal value)
