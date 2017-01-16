--TODO[react]: Find better place for this module
module Luna.Studio.State.StatefulAction where

import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Action   (Action)
import           Luna.Studio.State.Global   (State)
import qualified Luna.Studio.State.Global   as Global

class StatefulAction a where
    start :: a -> Command State ()
    start state = exitPrevious >> Global.performedAction ?= pack state
    update :: a -> Command State ()
    update state = Global.performedAction ?= pack state
    continue :: (a -> Command State ()) -> Command State ()
    continue action = do
        mayPerformedAction <- use $ Global.performedAction
        withJust mayPerformedAction $ \performedAction -> do
            withJust (matchState performedAction) action
    matchState :: Action -> Maybe a
    pack :: a -> Action
    exit :: a -> Command State ()

--TODO[react]: This should be replaced so it can use exit
exitPrevious :: Command State ()
exitPrevious = do
    mayPerformedAction <- use $ Global.performedAction
    -- withJust mayPerformedAction $ \performedAction -> do
    --     withJust (matchState performedAction) exit
    Global.performedAction .= Nothing
