module Test.Hspec.Empire (module Test.Hspec.Empire, module X) where

import Test.Hspec.Empire.Graph     as X
import Test.Hspec.Hspec.WithReason as X

import Empire.Prelude

import qualified Data.Graph.Store              as Store
import qualified Data.Text                     as Text
import qualified Empire.Commands.Graph         as Graph
import qualified Empire.Commands.GraphBuilder  as GraphBuilder
import qualified Empire.Commands.Typecheck     as Typecheck
import qualified Empire.Data.Graph             as Graph
import qualified Empire.Data.Library           as Library
import qualified Empire.Empire                 as Empire
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified System.IO.Temp                as Temp

import Control.Concurrent.MVar         (newEmptyMVar)
import Control.Concurrent.STM          (atomically)
import Control.Concurrent.STM.TChan    (newTChan)
import Control.Exception               (bracket)
import Data.Char                       (isSpace)
import Data.List                       (dropWhileEnd)
import Empire.ASTOp                    (runASTOp)
import Empire.Commands.Library         (createLibrary,
                                        withLibrary)
import Empire.Data.Graph               (CommandState (CommandState),
                                        defaultPMState)
import Empire.Empire                   (CommunicationEnv (CommunicationEnv),
                                        Empire, Env, evalEmpire, execEmpire,
                                        runEmpire)
import Empire.Empire                   (InterpreterEnv (InterpreterEnv))
import Luna.Package.Structure.Generate (genPackageStructure)
import LunaStudio.Data.GraphLocation   (GraphLocation (GraphLocation), (|>=))
import System.FilePath                 ((</>))
import Test.Hspec                      (Expectation, Spec, SpecWith, around,
                                        describe, parallel, shouldBe)
import Text.RawString.QQ               (r)


withChannels :: (CommunicationEnv -> IO a) -> IO a
withChannels = bracket createChannels (const $ pure ()) where
    createChannels = CommunicationEnv
        <$> atomically newTChan <*> newEmptyMVar <*> newEmptyMVar

runTests :: String -> SpecWith CommunicationEnv -> Spec
runTests = around withChannels . parallel .: describe

emptyCodeTemplate :: Text
emptyCodeTemplate = [r|
import Std.Base

def main:
    None
|]

normalizeLunaCode :: Text -> Text
normalizeLunaCode str = Text.intercalate "\n" $ Text.drop minWs <$> allLines where
    trimTrailingSpaces = Text.dropWhileEnd isSpace
    trimEmptyLines     = dropWhileEnd Text.null . dropWhile Text.null
    indentLength       = Text.length . Text.takeWhile isSpace
    allLines = trimEmptyLines $ trimTrailingSpaces <$> Text.lines str
    minWs    = minimum $ indentLength <$> filter (not . Text.null) allLines

codeCheck :: Text -> (Text -> Expectation)
codeCheck expectedCode = \resultCode ->
    Text.strip resultCode `shouldBe` normalizeLunaCode expectedCode

noAction :: GraphLocation -> Empire ()
noAction _ = pure ()

evalEmpireWithDefaultState :: CommunicationEnv -> Empire a -> IO a
evalEmpireWithDefaultState = fmap fst .: runEmpireWithDefaultState

execEmpireWithDefaultState
    :: CommunicationEnv -> Empire a -> IO (CommandState Env)
execEmpireWithDefaultState = fmap snd .: runEmpireWithDefaultState

runEmpireWithDefaultState
    :: CommunicationEnv -> Empire a -> IO (a, CommandState Env)
runEmpireWithDefaultState env action = defaultPMState >>= \pm ->
    runEmpire env (CommandState pm def) action


testCase
    :: Text
    -> Text
    -> (GraphLocation -> Empire a)
    -> CommunicationEnv
    -> Expectation
testCase initialCode expectedCode action env = do
    resultCode <- evalEmpireWithDefaultState env $ do
        gl <- prepareTestEnvironment initialCode
        action gl
        Graph.getCode gl
    codeCheck expectedCode resultCode

--[TODO]: This function is copy paste of testCase and is meant to be removed soon, when markers are removed from Luna
testCaseWithMarkers
    :: Text
    -> Text
    -> (GraphLocation -> Empire a)
    -> CommunicationEnv
    -> Expectation
testCaseWithMarkers initialCode expectedCode action env = do
    resultCode <- evalEmpireWithDefaultState env $ do
        gl <- prepareTestEnvironment initialCode
        action gl
        Graph.withGraph gl (use Graph.code)
    codeCheck expectedCode resultCode

testCaseWithTC
    :: Text
    -> Text
    -> (GraphLocation -> Empire a)
    -> (GraphLocation -> GraphBuilder.TCFunResolver -> Empire b)
    -> CommunicationEnv
    -> Expectation
testCaseWithTC initialCode expectedCode action tcResultCheck env = let
    projectParentDirName = "luna-test-hspec-empire-tc"
    inTempDirectory = Temp.withSystemTempDirectory projectParentDirName
    in inTempDirectory $ \projectParentDirNamePath -> do
        ((gl, resultCode, clsGraph, rooted), state)
            <- runEmpireWithDefaultState env $ do
                gl <- prepareTestEnvironmentWithTC
                    projectParentDirNamePath
                    initialCode
                let topGl = gl & GraphLocation.breadcrumb .~ def
                action gl
                code <- Graph.getCode gl
                Graph.withUnit topGl $ do
                    clsGraph <- use Graph.userState
                    rooted <- runASTOp . Store.serializeWithRedirectMap
                        $ clsGraph ^. Graph.clsClass
                    pure (gl, code, clsGraph, rooted)
        codeCheck expectedCode resultCode
        pmState <- Graph.defaultPMState
        let commandState = CommandState pmState
                $ InterpreterEnv (pure ()) clsGraph mempty def def def def
        updatedState <- execEmpire env commandState $
            Typecheck.runNoCleanUp gl clsGraph rooted False False
        let updatedClsGraph
                = updatedState ^. Graph.userState . Empire.clsGraph
            mappedUnits = updatedState ^. Graph.userState . Empire.mappedUnits
            resolveFun  = Typecheck.resolveSymbol mappedUnits
        void . evalEmpire env state $ do
            withLibrary (gl ^. GraphLocation.filePath)
                $ Graph.userState . Library.body .= updatedClsGraph
            tcResultCheck gl resolveFun

prepareTestEnvironment :: Text -> Empire GraphLocation
prepareTestEnvironment = prepareTestEnvironmentWithCustomPath defProjectPath where
    defProjectPath = "/TestProject"

prepareTestEnvironmentWithTC :: FilePath -> Text -> Empire GraphLocation
prepareTestEnvironmentWithTC projectParentDirName initialCode = let
    projectName             = "TestProject"
    genProjectPath dirPath  = dirPath </> projectName
    srcDirName              = "src"
    mainLunaName            = "Main.luna"
    genMainLunaPath pkgPath = pkgPath </> srcDirName </> mainLunaName
    in do
        Right pkgPath <- genPackageStructure
            (genProjectPath projectParentDirName)
            Nothing
            def
        prepareTestEnvironmentWithCustomPath
            (genMainLunaPath pkgPath)
            initialCode

prepareTestEnvironmentWithCustomPath :: FilePath -> Text -> Empire GraphLocation
prepareTestEnvironmentWithCustomPath filePath initialCode = let
    topGl               = GraphLocation filePath def
    mainNodeName        = "main"
    withMain mainNodeId = do
        let gl = topGl |>= mainNodeId
        mockNodesLayout gl
        pure gl
    in do
        createLibrary Nothing filePath
        Graph.loadCode topGl $ normalizeLunaCode initialCode
        mainNodeId <- findNodeIdByName topGl mainNodeName
        maybe (pure topGl) withMain mainNodeId
