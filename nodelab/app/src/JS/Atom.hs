{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( onEvent
    , pushCode
    -- , subscribeEventListenerInternal
    ) where


import qualified Data.List                  as List
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure         (pFromJSVal)
import           Text.Read                  (readMaybe)

import           Luna.Studio.Event.Event    (Event (Shortcut, UI))
import           Luna.Studio.Event.Internal (InternalEvent)
import           Luna.Studio.Event.Internal as Internal
import qualified Luna.Studio.Event.Shortcut as Shortcut
import           Luna.Studio.Event.UI       (UIEvent (SearcherEvent))
import           Luna.Studio.Prelude


foreign import javascript safe "atomCallback.pushCode($1)"
    pushCode :: JSString -> IO ()

foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallback.subscribeEventListenerInternal($1)"
  subscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unsubscribeEventListenerInternal()"
  unsubscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

onEvent :: (Event -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ mapM_ callback . parseEvent . pFromJSVal
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback

parseEvent :: String -> Maybe Event
parseEvent str = do
    let strBreak s = List.break (== ' ') s & _2 %~ drop 1
        (tpeStr, r)          = strBreak str
        (commandStr, argStr) = strBreak r
    case tpeStr of
        "Shortcut" -> Shortcut .: Shortcut.Event <$> readMaybe commandStr
                                                 <*> pure (if null argStr then Nothing else Just $ convert argStr)
        "Searcher" -> UI . SearcherEvent <$> readMaybe commandStr
        _          -> Nothing


-- subscribeEventListenerInternal :: (InternalEvent -> IO ()) -> IO (IO ())
-- subscribeEventListenerInternal callback = do
--     wrappedCallback <- syncCallback1 ContinueAsync $ callback . fromJSVal
--     subscribeEventListenerInternal' wrappedCallback
--     return $ unsubscribeEventListenerInternal' wrappedCallback >> releaseCallback wrappedCallback
