module TextEditor.Handler.Backend.Control
    ( handle
    ) where

-- import           JS.Atom                    (pushNotification)
import           TextEditor.Error.Error

import           Common.Prelude

import qualified TextEditor.Event.Batch    as Batch
import           TextEditor.Event.Event    (Event (Batch))

import           Common.Action.Command (Command)
import           TextEditor.State.Global   (State)



handle :: Event -> Maybe (Command State ())
handle (Batch (Batch.EmpireStarted _)) = Just $
    liftIO $ putStrLn "Server crashed."
    -- error "Server crashed." -- could have done that more politely, but… let it crash

handle _ = Nothing
