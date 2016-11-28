module UndoListSpec (
spec
) where
import           Test.Hspec (around, describe, expectationFailure, it, shouldSatisfy)




spec :: Spec
spec = describe "ctrlZ" $ do
    it "adds node and checks elements of undo list"
    u1 <- nextRandom
    (res, state) <- runEmpire env def $ do
        (pid, _) <- createProject Nothing "testProject"
        (lid, _) <- createLibrary pid (Just "ble") "/ble/ble"
        n1 <- Graph.addNode loc u1 "whatev" def

    return ()
