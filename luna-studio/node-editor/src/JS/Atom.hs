{-# LANGUAGE JavaScriptFFI #-}
module JS.Atom
    ( acceptEvent
    , setActiveLocation
    , onEvent
    ) where

import           Common.Prelude
import           Common.Report                 (error)
import           Data.Aeson                    (Result (Success), fromJSON)
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy.Char8    as BS
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal                 (fromJSVal, toJSVal_aeson)
import           GHCJS.Types                   (JSVal)
import qualified JS.Event                      as JS
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           NodeEditor.Event.Event        (Event (Atom, Shortcut, UI))
import           NodeEditor.Event.UI           (UIEvent (SearcherEvent))


foreign import javascript safe "atomCallback.onEvent($1)"
    onEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.unOnEvent()"
    unOnEvent' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallback.setActiveLocation($1)"
    setActiveLocation' :: JSVal -> IO ()

foreign import javascript safe "atomCallback.acceptEvent($1)"
    acceptEvent' :: JSString -> Bool

onEvent :: (Event -> IO ()) -> IO (IO ())
onEvent callback = do
    wrappedCallback <- syncCallback1 ContinueAsync
        $ mapM_ callback <=< parseEvent
    onEvent' wrappedCallback
    return $ unOnEvent' wrappedCallback >> releaseCallback wrappedCallback

parseEvent :: JSVal -> IO (Maybe Event)
parseEvent jsval = do
    fromJSVal jsval >>= \case
        Just value -> do
            case (Atom <$> fromJSON value)
                <> ((UI . SearcherEvent) <$> fromJSON value)
                <> (Shortcut <$> fromJSON value
                ) of
                    Success r -> return $ Just r
                    _ -> error "Unparseable event" >> return Nothing
        Nothing -> error "Unknown event" >> return Nothing

setActiveLocation :: MonadIO m => GraphLocation -> m ()
setActiveLocation gl = liftIO $ setActiveLocation' =<< toJSVal_aeson gl

acceptEvent :: JS.Event -> Bool
acceptEvent = acceptEvent' . convert . BS.unpack . Aeson.encode
