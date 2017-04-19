module Luna.Atom.Event.Preprocessor.CustomEvent
    ( process
    ) where

import           Luna.Atom.Event.CustomEvent as CustomEvent
import           Luna.Atom.Event.Debug       (Event (..))
import qualified Luna.Atom.Event.Event       as Event
import           Luna.Prelude


-- TODO[LJK, PM]: Unused code
-- import           Data.Aeson                    (fromJSON)
-- import qualified Data.Aeson                    as AE
--
-- payloadToData :: (AE.FromJSON a) => String -> JSVal -> IO (Maybe a)
-- payloadToData topic payload = do
--     d <- fromJSVal payload
--     case d of
--         Nothing -> return Nothing
--         Just v -> case fromJSON v of
--             AE.Success v' -> return $ Just v'
--             AE.Error msg -> do
--                 putStrLn $ "Malformed JS data [" <> topic <> "]: " <> msg
--                 return Nothing
--

process :: Event.Event -> IO (Maybe Event.Event)
process (Event.CustomEvent (CustomEvent.RawEvent topic _)) = case topic of
    "debug.getState" -> return $ Just $ Event.Debug GetState
    "tick"           -> return $ Just Event.Tick
    _                -> return Nothing
process _ = return Nothing
