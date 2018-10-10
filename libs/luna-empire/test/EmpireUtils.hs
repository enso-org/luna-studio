module EmpireUtils (module EmpireUtils, module X) where

import Empire.Empire                 as X (runEmpire)
import Test.Hspec.Empire             as X

import Empire.Prelude

import qualified Empire.Commands.Graph as Graph
import qualified Empire.Data.Library   as Library (body, path)
import qualified LunaStudio.Data.Node  as Node

import Data.Reflection               (Given (..), give)
import Empire.Commands.Library       (listLibraries, withLibrary)
import Empire.Commands.Library       (createLibrary)
import Empire.Data.Graph             (ClsGraph, CommandState (..),
                                      defaultPMState, userState)
import Empire.Empire                 (CommunicationEnv (..), Empire, Env,
                                      InterpreterEnv (..))
import LunaStudio.Data.Breadcrumb    (Breadcrumb (Breadcrumb))
import LunaStudio.Data.GraphLocation (GraphLocation (GraphLocation), (|>=))
import LunaStudio.Data.Node          (NodeId, nodeId)
import Test.Hspec                    (Expectation)


runEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO (a, CommandState Env)
runEmp env act = defaultPMState >>= \pm ->
    runEmpire env (CommandState pm def) $ do
        let testFile = "/TestFile"
            topGl    = GraphLocation testFile def
        void $ createLibrary (Just testFile) testFile
        Graph.loadCode topGl "def main:\n    None"
        [node] <- Graph.getNodes topGl
        give (topGl |>= (node ^. Node.nodeId)) act

evalEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO a
evalEmp env act = fst <$> runEmp env act

runEmp'
    :: CommunicationEnv
    -> CommandState Env
    -> ClsGraph
    -> (Given GraphLocation => Empire a)
    -> IO (a, CommandState Env)
runEmp' env st newGraph act = runEmpire env st $ do
    lib <- head <$> listLibraries
    let path = lib ^. Library.path
    withLibrary path $ userState . Library.body .= newGraph
    let toLoc = GraphLocation path
    give (toLoc $ Breadcrumb []) act

graphIDs :: GraphLocation -> Empire [NodeId]
graphIDs loc = do
    nodes <- Graph.getNodes loc
    let ids = fmap (^. nodeId) nodes
    return ids

extractGraph :: CommandState InterpreterEnv -> ClsGraph
extractGraph = _clsGraph . view userState

withResult :: a -> (a -> IO b) -> IO b
withResult res act = act res

top :: Given GraphLocation => GraphLocation
top = given

emptyGraphLocation :: GraphLocation
emptyGraphLocation = GraphLocation "" $ Breadcrumb []

--[TODO]: This function exists for backwards compatibility, meant to be removed soon
specifyCodeChange :: Text -> Text -> (GraphLocation -> Empire a) -> CommunicationEnv -> Expectation
specifyCodeChange initialCode expectedCode action env =
    testCase initialCode expectedCode action env

