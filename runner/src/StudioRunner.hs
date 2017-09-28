{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import           Prelude                       hiding (FilePath)
import           Control.Lens.Aeson
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.State.Layered
import           Data.ByteString.Lazy          (unpack)
import           Data.List.Split
import qualified Data.List                     as List
import           Data.Maybe                    (fromMaybe, maybeToList)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS     (decodeString, encodeString, fromText)
import           Options.Applicative
import           System.Directory              (doesDirectoryExist, setCurrentDirectory, getHomeDirectory, getCurrentDirectory, createDirectoryIfMissing)
import           System.Exit                   (ExitCode)
import           System.Process.Typed          (shell, runProcess, runProcess_, setWorkingDir, readProcess_)
import           System.Environment            (getExecutablePath, getArgs)
import qualified System.Environment            as Environment
import qualified System.IO                     as IO
import qualified Shelly.Lifted                 as Shelly
import           System.Host

default (T.Text)

puts :: (MonadIO m, Show s) => s -> m ()
puts x = liftIO $ do
    print x
    IO.hFlush IO.stdout

data RunnerConfig = RunnerConfig { _versionFile            :: FilePath
                                 , _mainHomeDir            :: FilePath
                                 , _userConfigFolder       :: FilePath
                                 , _configFolder           :: FilePath
                                 , _configHomeFolder       :: FilePath
                                 , _studioHome             :: FilePath
                                 , _logsFolder             :: FilePath
                                 , _atomPackageName        :: FilePath
                                 , _appName                :: FilePath
                                 , _supervisorFolder       :: FilePath
                                 , _supervisordFolder      :: FilePath
                                 , _supervisordBin         :: FilePath
                                 , _supervisordConfig      :: FilePath
                                 , _atomFolder             :: FilePath
                                 , _thirdPartyFolder       :: FilePath
                                 , _backendBinsFolder      :: FilePath
                                 , _binsFolder             :: FilePath
                                 , _packageFolder          :: FilePath
                                 , _supervisorKillFolder   :: FilePath
                                 , _supervisorKillBin      :: FilePath
                                 , _atomPath               :: FilePath
                                 }

makeLenses ''RunnerConfig

type MonadRun m = (MonadStates '[RunnerConfig] m, MonadIO m)

instance Monad m => MonadHostConfig RunnerConfig 'Linux arch m where
    defaultHostConfig = return $ RunnerConfig
        { _versionFile            = "version.txt"
        , _mainHomeDir            = ".luna"
        , _userConfigFolder       = "user-config"
        , _configFolder           = "config"
        , _configHomeFolder       = "config"
        , _studioHome             = "atom"
        , _logsFolder             = "logs"
        , _atomPackageName        = "luna-studio"
        , _appName                = "luna-studio"
        , _supervisorFolder       = "supervisor"
        , _supervisordFolder      = "supervisord"
        , _supervisordBin         = "supervisord"
        , _supervisordConfig      = "supervisord-linux.conf"
        , _atomFolder             = "atom"
        , _thirdPartyFolder       = "third-party"
        , _backendBinsFolder      = "private"
        , _binsFolder             = "bin"
        , _packageFolder          = "packages"
        , _supervisorKillFolder   = "kill"
        , _supervisorKillBin      = "kill"
        , _atomPath               = "atom" </> "usr" </> "bin" </> "atom"
        }

instance Monad m => MonadHostConfig RunnerConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & atomPath          .~ ("Atom.app" </> "Contents" </> "MacOS" </> "Atom")
                           & supervisordConfig .~ "supervisord-mac.conf"

instance Monad m => MonadHostConfig RunnerConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & atomPath .~ ("Atom" </> "atom.exe")

-- path helpers --
scriptDir, mainAppDir :: MonadIO m => m FilePath
scriptDir  = (directory . decodeString) <$> liftIO getExecutablePath
mainAppDir = (parent . parent . parent) <$> scriptDir

relativeToDir :: MonadRun m => m FilePath -> [Getting FilePath RunnerConfig FilePath] -> m FilePath
relativeToDir basePath segmentAccessors = do
    runnerCfg <- get @RunnerConfig
    base      <- basePath
    let pathSegments = map (runnerCfg ^.) segmentAccessors
    return $ foldl (</>) base pathSegments

relativeToMainDir, relativeToHomeDir :: MonadRun m => [Getting FilePath RunnerConfig FilePath] -> m FilePath
relativeToMainDir segmentAccessors = relativeToDir mainAppDir segmentAccessors
relativeToHomeDir segmentAccessors = relativeToDir (decodeString <$> (liftIO $ getHomeDirectory)) (mainHomeDir : segmentAccessors)

version :: MonadRun m => m FilePath
version = do
    versionFilePath <- relativeToMainDir [configFolder, versionFile]
    versionStr      <- liftIO $ readFile $ encodeString versionFilePath
    return . fromText . T.pack $ versionStr

-- paths --
backendBinsPath, configPath, atomAppPath, backendDir             :: MonadRun m => m FilePath
supervisordBinPath, killSupervisorBinPath, packageStudioAtomHome :: MonadRun m => m FilePath
userStudioAtomHome, localLogsDirectory, userLogsDirectory        :: MonadRun m => m FilePath

backendBinsPath       = relativeToMainDir [binsFolder, backendBinsFolder]
configPath            = relativeToMainDir [configFolder]
atomAppPath           = relativeToMainDir [thirdPartyFolder, atomPath]
backendDir            = relativeToMainDir [configFolder, supervisorFolder]
supervisordBinPath    = relativeToMainDir [thirdPartyFolder, supervisordFolder,    supervisordBin]
killSupervisorBinPath = relativeToMainDir [thirdPartyFolder, supervisorKillFolder, supervisorKillBin]
packageStudioAtomHome = relativeToMainDir [userConfigFolder, studioHome]
localLogsDirectory    = relativeToMainDir [logsFolder]
userStudioAtomHome    = relativeToHomeDir [configHomeFolder, appName] >>= (\p -> (fmap (p </>) version))
userLogsDirectory     = relativeToHomeDir [logsFolder,       appName] >>= (\p -> (fmap (p </>) version))

-- misc runner utils --
copyLunaStudio :: MonadRun m => m ()
copyLunaStudio = do
    packageAtomHome <- packageStudioAtomHome
    atomHome <- userStudioAtomHome
    puts "[copyLunaStudio] packageAtomHome"
    puts packageAtomHome
    puts "[copyLunaStudio] atomHome"
    puts atomHome
    Shelly.shelly $ Shelly.mkdir_p atomHome
    Shelly.shelly $ Shelly.cp_r packageAtomHome atomHome

testDirectory :: MonadIO m => FilePath -> m Bool
testDirectory path = Shelly.shelly $ Shelly.test_d path

checkLunaHome :: MonadRun m => m ()
checkLunaHome = do
    runnerCfg       <- get @RunnerConfig
    userAtomHome    <- userStudioAtomHome
    let pathLunaPackage = userAtomHome </> (runnerCfg ^. packageFolder) </> (runnerCfg ^. atomPackageName)
    puts "[checkLunaHome] pathLunaPackage"
    puts pathLunaPackage
    testDirectory pathLunaPackage >>= \exists ->
        unless exists $ puts "[!!!!] Creating a directory" >> copyLunaStudio

setEnv :: MonadRun m => String -> FilePath -> m ()
setEnv name path = liftIO $ Environment.setEnv name $ encodeString path

unixOnly :: MonadRun m => m () -> m ()
unixOnly act = case currentHost of
    Windows -> liftIO $ putStrLn "Unsupported system (Windows)"
    _       -> act

-- run functions --
runLunaEmpire :: MonadRun m => FilePath -> m ()
runLunaEmpire configFile = do
    logs           <- localLogsDirectory
    lunaSupervisor <- backendDir
    supervisord    <- supervisordBinPath
    Shelly.shelly $ Shelly.mkdir_p logs
    case currentHost of
        Linux  -> Shelly.shelly $ do
            Shelly.cd lunaSupervisor
            Shelly.cmd supervisord "-n" "-c" configFile
        Darwin -> runProcess_ $ setWorkingDir (encodeString lunaSupervisor)
                              $ shell ((encodeString supervisord) ++ " -n -c " ++ (encodeString configFile))
                              -- done with system.process.typed because with shelly clicking on app in launchpad returned abnormal exit code

runFrontend :: MonadRun m => Maybe T.Text -> m ()
runFrontend args = do
    atomHome    <- packageStudioAtomHome
    atom        <- atomAppPath
    liftIO $ Environment.setEnv "LUNA_STUDIO_DEVELOP" "True"
    liftIO $ Environment.setEnv "ATOM_HOME" (encodeString $ atomHome )
    unixOnly $ Shelly.shelly $ Shelly.run_ atom $ "-w" : (maybeToList args)

runBackend :: MonadRun m => m ()
runBackend = do
    setEnv "LUNA_STUDIO_LOG_PATH"     =<< localLogsDirectory
    setEnv "LUNA_STUDIO_BACKEND_PATH" =<< backendBinsPath
    setEnv "LUNA_STUDIO_CONFIG_PATH"  =<< configPath
    unixOnly $ runLunaEmpire "supervisord.conf"

runLocal :: MonadRun m => m ()
runLocal = do
    runnerCfg <- get @RunnerConfig
    setEnv "LUNA_STUDIO_GUI_CONFIG_PATH" =<< packageStudioAtomHome
    setEnv "LUNA_STUDIO_LOG_PATH"        =<< localLogsDirectory
    setEnv "LUNA_STUDIO_BACKEND_PATH"    =<< backendBinsPath
    setEnv "LUNA_STUDIO_GUI_PATH"        =<< atomAppPath
    setEnv "LUNA_STUDIO_CONFIG_PATH"     =<< configPath
    setEnv "LUNA_STUDIO_KILL_PATH"       =<< killSupervisorBinPath
    unixOnly $ runLunaEmpire $ runnerCfg ^. supervisordConfig

runPackage :: MonadRun m => m ()
runPackage = case currentHost of
    Windows -> do
        atom <- atomAppPath
        setEnv "ATOM_HOME" =<< ((</> "atom") <$> userStudioAtomHome)
        checkLunaHome
        Shelly.shelly $ Shelly.cmd atom
    _ -> do
        runnerCfg <- get @RunnerConfig
        setEnv "LUNA_STUDIO_GUI_CONFIG_PATH" =<< ((</> "atom") <$> userStudioAtomHome)
        setEnv "LUNA_STUDIO_LOG_PATH"        =<< userLogsDirectory
        setEnv "LUNA_STUDIO_BACKEND_PATH"    =<< backendBinsPath
        setEnv "LUNA_STUDIO_GUI_PATH"        =<< atomAppPath
        setEnv "LUNA_STUDIO_CONFIG_PATH"     =<< configPath
        setEnv "LUNA_STUDIO_KILL_PATH"       =<< killSupervisorBinPath
        checkLunaHome
        runLunaEmpire $ runnerCfg ^. supervisordConfig

runApp :: MonadRun m => Bool -> Maybe String -> m ()
runApp develop atom = do
    liftIO $ Environment.setEnv "LUNA_STUDIO_ATOM_ARG" (fromMaybe " " atom)
    if develop then runLocal else runPackage


run :: MonadIO m => Options -> m ()
run (Options frontend backend develop atom) = evalDefHostConfigs @'[RunnerConfig] $
    if  frontend
        then runFrontend $ T.pack <$> atom
    else if backend
        then runBackend
    else runApp develop atom

-- option parser --
data Options = Options
    { frontend :: Bool
    , backend  :: Bool
    , develop  :: Bool
    , atom     :: Maybe String} deriving Show

optionParser :: Parser Options
optionParser = Options
    <$> switch (long "frontend" <> short 'f')
    <*> switch (long "backend" <> short 'b')
    <*> switch (long "develop" <> short 'd')
    <*> (optional $ strOption $ long "atom" <> short 'a')

filterArg :: String -> Bool
filterArg arg = not $ List.isInfixOf "-psn" arg

filterArgs :: [String] -> [String]
filterArgs = filter filterArg

filteredParser :: ParserPrefs -> ParserInfo a -> IO a
filteredParser pprefs pinfo = execParserPure pprefs pinfo . filterArgs <$> getArgs >>= handleParseResult

parser :: MonadIO m => m Options
parser = liftIO $ filteredParser p opts
    where
        opts = info (optionParser <**> helper) idm
        p = prefs showHelpOnEmpty

main :: IO ()
main = run =<< parser
