{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( onEvent
    , pushCode
    , pushNotification
    -- , subscribeEventListenerInternal
    ) where
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure         (pFromJSVal)
import           GHCJS.Types                (JSVal)
import           Luna.Studio.Error.Error
import qualified Luna.Studio.Event.Shortcut as Shortcut

import qualified Data.List                  as List
import           Text.Read                  (readMaybe)

import           Luna.Studio.Event.Event    (Event (Shortcut, UI))
import           Luna.Studio.Event.UI       (UIEvent (SearcherEvent))
import           Luna.Studio.Prelude


foreign import javascript safe "atomCallback.pushNotification($1, $2)"
  pushNotification' :: Int -> JSString -> IO ()

foreign import javascript safe "atomCallback.pushCode($1)"
    pushCode :: JSString -> IO ()

foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()

onEvent :: (Event -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ mapM_ callback . parseEvent . pFromJSVal
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback

pushNotification :: Notification -> IO ()
pushNotification  = do
    num <- (^. notificationType)
    msg <- (^. notificationMsg)
    return $ pushNotification' (fromEnum num) (convert msg)

parseEvent :: String -> Maybe Event
parseEvent str = do
    let strBreak s = List.break (== ' ') s & _2 %~ drop 1
        (tpeStr, r) = strBreak str
    case tpeStr of
        "Shortcut" -> do let (commandStr, argStr) = strBreak r
                         Shortcut .: Shortcut.Event <$> readMaybe commandStr
                                                    <*> pure (if null argStr then Nothing else Just $ convert argStr)
        "Searcher" -> UI . SearcherEvent <$> readMaybe r
        _          -> Nothing


-- subscribeEventListenerInternal :: (InternalEvent -> IO ()) -> IO (IO ())
-- subscribeEventListenerInternal callback = do
--     wrappedCallback <- syncCallback1 ContinueAsync $ callback . fromJSVal
--     subscribeEventListenerInternal' wrappedCallback
--     return $ unsubscribeEventListenerInternal' wrappedCallback >> releaseCallback wrappedCallback
