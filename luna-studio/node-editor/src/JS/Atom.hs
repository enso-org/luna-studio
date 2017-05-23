{-# LANGUAGE JavaScriptFFI #-}
module JS.Atom
    ( onEvent
    ) where
import           Common.Prelude
import           Data.Aeson             (Result (Success), fromJSON)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal          (fromJSVal)
import           GHCJS.Types            (JSVal)
import           NodeEditor.Event.Event (Event (Atom, Shortcut, UI))
import           NodeEditor.Event.UI    (UIEvent (SearcherEvent))


foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()

onEvent :: (Event -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ mapM_ callback <=< parseEvent
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback

parseEvent :: JSVal -> IO (Maybe Event)
parseEvent jsval = do
    fromJSVal jsval >>= \case
        Just value -> return $ do
            case (Atom <$> fromJSON value) <> ((UI . SearcherEvent) <$> fromJSON value) <> (Shortcut <$> fromJSON value) of
                Success r -> Just r
                _ -> Nothing
        Nothing -> return Nothing
