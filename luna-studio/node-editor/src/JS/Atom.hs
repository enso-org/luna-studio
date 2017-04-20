{-# LANGUAGE JavaScriptFFI #-}
module JS.Atom
    ( onEvent
    ) where
import qualified Data.List                     as List
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure            (pFromJSVal)
import           GHCJS.Types                   (JSVal)
import           NodeEditor.Event.Event       (Event (Shortcut, UI))
import qualified NodeEditor.Event.Shortcut    as Shortcut
import           NodeEditor.Event.UI          (UIEvent (SearcherEvent))
import           Common.Prelude
import           Text.Read                     (readMaybe)


foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()

onEvent :: (Event -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ mapM_ callback . parseEvent . pFromJSVal
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback

parseEvent :: String -> Maybe Event
parseEvent str = do
    let strBreak s = List.break (== ' ') s & _2 %~ drop 1
        (tpeStr, r) = strBreak str
    case tpeStr of
        "Shortcut" -> do let (commandStr, argStr) = strBreak r
                         Shortcut .: Shortcut.Event <$> readMaybe commandStr
                                                    <*> pure (if null argStr then Nothing else Just argStr)
        "Searcher" -> UI . SearcherEvent <$> readMaybe r
        _          -> Nothing
