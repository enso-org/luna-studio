module Luna.Studio.Event.Preprocessor.CustomEvent (process) where

import           Data.Aeson                    (fromJSON)
import qualified Data.Aeson                    as AE
import           Luna.Studio.Prelude

import           Luna.Studio.Event.CustomEvent as CustomEvent
import           Luna.Studio.Event.Debug       (Event (..))
import qualified Luna.Studio.Event.Event       as Event



payloadToData :: (AE.FromJSON a) => String -> JSVal -> IO (Maybe a)
payloadToData topic payload = do
    d <- fromJSVal payload
    case d of
        Nothing -> return Nothing
        Just v -> case fromJSON v of
            AE.Success v' -> return $ Just v'
            AE.Error msg -> do
                putStrLn $ "Malformed JS data [" <> topic <> "]: " <> msg
                return Nothing

process :: Event.Event -> IO (Maybe Event.Event)
process (Event.CustomEvent (CustomEvent.RawEvent topic payload)) = case topic of
    "debug.getState" -> return $ Just $ Event.Debug GetState
    "tick"           -> return $ Just Event.Tick
    _                -> return Nothing
process _ = return Nothing
