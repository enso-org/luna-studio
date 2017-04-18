module Internal.Handler.Backend.Control
    ( handle
    ) where

-- import           JS.Atom                    (pushNotification)
import           Internal.Error.Error

import           Luna.Prelude

import qualified Internal.Event.Batch    as Batch
import           Internal.Event.Event    (Event (Batch))

import           Internal.Action.Command (Command)
import           Internal.State.Global   (State)



handle :: Event -> Maybe (Command State ())
handle (Batch (Batch.EmpireStarted _)) = Just $
    liftIO $ putStrLn "Server crashed."
    -- error "Server crashed." -- could have done that more politely, but… let it crash

handle _ = Nothing
