module NodeEditor.Action.State.Action where

import           Common.Action.Command   (Command)
import           Common.Prelude
import qualified Data.Map                as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           NodeEditor.State.Action (Action (end, update), ActionRep, SomeAction, fromSomeAction, overlappingActions, someAction)
import           NodeEditor.State.Global (State, actions, currentActions)


checkSomeAction :: ActionRep -> Command State (Maybe (SomeAction (Command State)))
checkSomeAction actionRep = Map.lookup actionRep <$> use (actions . currentActions)

checkAction :: Action (Command State) a => ActionRep -> Command State (Maybe a)
checkAction actionRep = do
    maySomeAction <- checkSomeAction actionRep
    return $ join $ fromSomeAction <$> maySomeAction

checkIfActionPerfoming :: ActionRep -> Command State Bool
checkIfActionPerfoming actionRep = Map.member actionRep <$> use (actions . currentActions)

runningActions :: Command State [ActionRep]
runningActions = Map.keys <$> use (actions . currentActions)

getCurrentOverlappingActions :: ActionRep -> Command State [SomeAction (Command State)]
getCurrentOverlappingActions a = do
    let checkOverlap :: ActionRep -> ActionRep -> Bool
        checkOverlap a1 a2 = any (Set.isSubsetOf (Set.fromList [a1, a2])) overlappingActions
        overlappingActionReps = filter (checkOverlap a) <$> runningActions
    ca <- use (actions . currentActions)
    catMaybes <$> map (flip Map.lookup ca) <$> overlappingActionReps

beginActionWithKey :: Action (Command State) a => ActionRep -> a -> Command State ()
beginActionWithKey key action = do
    currentOverlappingActions <- getCurrentOverlappingActions key
    mapM_ end currentOverlappingActions
    update action

continueActionWithKey :: Action (Command State) a => ActionRep -> (a -> Command State ()) -> Command State ()
continueActionWithKey key run = do
    maySomeAction <- use $ actions . currentActions . at key
    mapM_ run $ maySomeAction >>= fromSomeAction

updateActionWithKey :: Action (Command State) a => ActionRep -> a -> Command State ()
updateActionWithKey key action = actions . currentActions . at key ?= someAction action

removeActionFromState :: ActionRep -> Command State ()
removeActionFromState key = actions . currentActions %= Map.delete key

endActions :: Set ActionRep -> Command State ()
endActions reps = use (actions . currentActions) >>= mapM_ end . Map.filterWithKey (\k _ -> Set.member k reps)

endAllActions :: Command State [ActionRep]
endAllActions = use (actions . currentActions) >>= \actionsMap -> do
    mapM_ end actionsMap
    return $ Map.keys actionsMap
