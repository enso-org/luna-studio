module Luna.Studio.Handler.Backend.Control
    ( handle
    ) where

import           JS.Atom (pushNotification)
import           Luna.Studio.Error.Error

import           Luna.Studio.Prelude

import qualified Luna.Studio.Event.Batch    as Batch
import           Luna.Studio.Event.Event    (Event (Batch))

import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.State.Global   (State)



handle :: Event -> Maybe (Command State ())
handle (Batch (Batch.EmpireStarted _)) = Just $
    liftIO $ pushNotification $ Notification Error "Server crashed."
    -- error "Server crashed." -- could have done that more politely, but… let it crash

handle _ = Nothing
