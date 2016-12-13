module Event.Processors.CustomEvent (process) where

import           Control.Monad     (liftM)
import           Data.Aeson        (fromJSON)
import qualified Data.Aeson        as AE
import           GHCJS.Marshal     (fromJSVal)
import           Luna.Studio.Prelude

import           Event.CustomEvent as CustomEvent
import           Event.Debug       (Event (..))
import qualified Event.Event       as Event

payloadToData :: (AE.FromJSON a) => String -> JSVal -> IO (Maybe a)
payloadToData topic payload = do
    d <- fromJSVal payload
    case d of
        Nothing -> return Nothing
        Just v -> case (fromJSON v) of
            AE.Success v' -> return $ Just v'
            AE.Error msg -> do
                putStrLn $ "Malformed JS data [" <> topic <> "]: " <> msg
                return Nothing

process :: Event.Event -> IO (Maybe Event.Event)
process (Event.CustomEvent (CustomEvent.RawEvent topic payload)) = case topic of
    "debug.getState" -> return $ Just $ Event.Debug $ GetState
    "nodesearcher"   -> (liftM Event.NodeSearcher) <$> payloadToData topic payload
    "tick"           -> return $ Just $ Event.Tick
    "widget"         -> (liftM Event.Widget) <$> payloadToData topic payload
    _               -> return Nothing
process _ = return Nothing
