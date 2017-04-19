module Luna.Atom.Handler.Backend.Control
    ( handle
    ) where

-- import           JS.Atom                    (pushNotification)
import           Luna.Atom.Error.Error

import           Luna.Prelude

import qualified Luna.Atom.Event.Batch    as Batch
import           Luna.Atom.Event.Event    (Event (Batch))

import           Luna.Atom.Action.Command (Command)
import           Luna.Atom.State.Global   (State)



handle :: Event -> Maybe (Command State ())
handle (Batch (Batch.EmpireStarted _)) = Just $
    liftIO $ putStrLn "Server crashed."
    -- error "Server crashed." -- could have done that more politely, but… let it crash

handle _ = Nothing
