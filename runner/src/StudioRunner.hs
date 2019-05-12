{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE FlexibleContexts      #-}

module Main where

import           Prelude
import           Control.Exception.Safe        (MonadMask, MonadCatch, bracket_, catch, catchAny)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy          as BL
import qualified Data.List                     as List
import           Data.Maybe                    (fromMaybe, maybeToList)
import           Data.Semigroup                ((<>))
import qualified "containers"  Data.Set        as Set
import           "containers"  Data.Set        (Set)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Options.Applicative
import           Path
import           Path.IO
import qualified System.FilePath               as FP
import           System.Exit                   (ExitCode)
import           System.Process.Typed          (proc, shell, runProcess_, setWorkingDir, readProcess_)
import           System.Environment            (getExecutablePath, getArgs, lookupEnv)
import qualified System.Environment            as Environment
import           System.IO.Error               (isDoesNotExistError)
import           System.Host

default (T.Text)


data RunnerConfig = RunnerConfig { _versionFile            :: Path Rel File
                                 , _mainHomeDir            :: Path Rel Dir
                                 , _userConfigFolder       :: Path Rel Dir
                                 , _configFolder           :: Path Rel Dir
                                 , _configHomeFolder       :: Path Rel Dir
                                 , _storageDataHomeFolder  :: Path Rel Dir
                                 , _studioHome             :: Path Rel Dir
                                 , _logsFolder             :: Path Rel Dir
                                 , _atomPackageName        :: Path Rel Dir
                                 , _appName                :: Path Rel Dir
                                 , _supervisorFolder       :: Path Rel Dir
                                 , _supervisordFolder      :: Path Rel Dir
                                 , _supervisordBin         :: Path Rel File
                                 , _supervisorctlBin       :: Path Rel File
                                 , _supervisordConfig      :: Path Rel File
                                 , _atomFolder             :: Path Rel Dir
                                 , _thirdPartyFolder       :: Path Rel Dir
                                 , _backendBinsFolder      :: Path Rel Dir
                                 , _binsFolder             :: Path Rel Dir
                                 , _packageFolder          :: Path Rel Dir
                                 , _supervisorKillFolder   :: Path Rel Dir
                                 , _supervisorKillBin      :: Path Rel File
                                 , _atomBinPath            :: Path Rel File
                                 , _mainTmpDirectoryTempl  :: String
                                 , _lunaProjects           :: Path Rel Dir
                                 , _tutorialsDirectory     :: Path Rel Dir
                                 , _userInfoFile           :: Path Rel File
                                 , _resourcesFolder        :: Path Rel Dir
                                 , _shareFolder            :: Path Rel Dir
                                 , _windowsFolder          :: Path Rel Dir
                                 }

makeLenses ''RunnerConfig

type MonadRun m = (MonadState RunnerConfig m, MonadIO m, MonadMask m)

instance Monad m => MonadHostConfig RunnerConfig 'Linux arch m where
    defaultHostConfig = return $ RunnerConfig
        { _versionFile            = $(mkRelFile "version.txt")
        , _mainHomeDir            = $(mkRelDir ".luna")
        , _userConfigFolder       = $(mkRelDir "user-config")
        , _configFolder           = $(mkRelDir "config")
        , _configHomeFolder       = $(mkRelDir "config")
        , _storageDataHomeFolder  = $(mkRelDir "storage")
        , _studioHome             = $(mkRelDir "atom")
        , _logsFolder             = $(mkRelDir "logs")
        , _atomPackageName        = $(mkRelDir "luna-studio")
        , _appName                = $(mkRelDir "luna-studio")
        , _supervisorFolder       = $(mkRelDir "supervisor")
        , _supervisordFolder      = $(mkRelDir "supervisord")
        , _supervisordBin         = $(mkRelFile "supervisord")
        , _supervisorctlBin       = $(mkRelFile "supervisorctl")
        , _supervisordConfig      = $(mkRelFile "supervisord-package.conf")
        , _atomFolder             = $(mkRelDir "atom")
        , _thirdPartyFolder       = $(mkRelDir "third-party")
        , _backendBinsFolder      = $(mkRelDir "private")
        , _binsFolder             = $(mkRelDir "bin")
        , _packageFolder          = $(mkRelDir "packages")
        , _supervisorKillFolder   = $(mkRelDir "kill")
        , _supervisorKillBin      = $(mkRelFile "kill")
        , _atomBinPath            = $(mkRelFile $ "atom" FP.</> "usr" FP.</> "bin" FP.</> "atom")
        , _mainTmpDirectoryTempl  = "luna"
        , _lunaProjects           = $(mkRelDir $ "luna" FP.</> "projects")
        , _tutorialsDirectory     = $(mkRelDir "tutorials")
        , _userInfoFile           = $(mkRelFile "user_info.json")
        , _resourcesFolder        = $(mkRelDir $ "public" FP.</> "luna-studio" FP.</> "resources")
        , _shareFolder            = $(mkRelDir $ ".local" FP.</> "share")
        , _windowsFolder          = $(mkRelDir "windows")
        }

instance Monad m => MonadHostConfig RunnerConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & atomBinPath .~ $(mkRelFile $ "Atom.app" FP.</> "Contents" FP.</> "MacOS" FP.</> "Atom")

instance Monad m => MonadHostConfig RunnerConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & atomBinPath .~ $(mkRelFile $ "Atom" FP.</> "atom.exe")

-- path helpers --
runnerDir :: MonadIO m => m (Path Abs Dir)
runnerDir = liftIO $ do
  x <- getExecutablePath
  parent <$> parseAbsFile x

absAppDir :: MonadIO m => m (Path Abs Dir)
absAppDir = do
    runnerDir <- runnerDir
    -- Due to certain issues, two copies of luna-studio might be shipped:
    -- `bin/main/luna-studio` and `bin/public/luna-studio/luna-studio`
    -- Thus, we need to check where are we, to say where is the package root.
    -- This workaround should be removed once this issue is addressed:
    -- https://github.com/luna/luna-manager/issues/226
    let stepUp = if dirname runnerDir == $(mkRelDir "main")
                 then parent . parent          -- drop bin/main/
                 else parent . parent . parent -- drop bin/public/luna-studio/
    pure $ stepUp runnerDir

absHomeDir ::  MonadRun m => m (Path Abs Dir)
absHomeDir = do
  runnerCfg <- get @RunnerConfig
  home      <- getHomeDir
  return $ home </> (runnerCfg ^. mainHomeDir)

relToDirDir :: MonadRun m
    => m (Path Abs Dir)
    -> [Getting (Path Rel Dir) RunnerConfig (Path Rel Dir)]
    -> m (Path Abs Dir)
relToDirDir basePath segmentAccessors = do
    runnerCfg <- get @RunnerConfig
    b <- basePath
    let segments = map (runnerCfg ^.) segmentAccessors
    pure $ foldl (</>) b segments

relToDirFile :: MonadRun m
    => m (Path Abs Dir)
    -> [Getting (Path Rel Dir) RunnerConfig (Path Rel Dir)]
    -> Getting (Path Rel File) RunnerConfig (Path Rel File)
    -> m (Path Abs File)
relToDirFile basePath segmentAccessors fileAccessor = do
    runnerCfg <- get @RunnerConfig
    d <- relToDirDir basePath segmentAccessors
    pure $ d </> (runnerCfg ^. fileAccessor)

versionText :: MonadRun m => m T.Text
versionText = T.pack <$> version

version :: MonadRun m => m String
version = do
    versionFP <- toFilePath <$> versionFilePath
    liftIO $ readFile versionFP

printVersion :: (MonadRun m, MonadCatch m) => m ()
printVersion = do
    versionTxt <- catch versionText $ \e -> pure $ if isDoesNotExistError e
                                                   then "develop"
                                                   else T.pack $ show e
    liftIO $ print versionTxt

-- paths --
backendBinsPath, configPath, backendDir                                  :: MonadRun m => m (Path Abs Dir)
packageStudioAtomHome, userStudioAtomHome, localLogsDirectory            :: MonadRun m => m (Path Abs Dir)
resourcesDirectory, windowsLogsDirectory, userLogsDirectory              :: MonadRun m => m (Path Abs Dir)
userdataStorageDirectory, localdataStorageDirectory, lunaProjectsPath    :: MonadRun m => m (Path Abs Dir)
sharePath, windowsScriptsPath, backendLdLibraryPath                      :: MonadRun m => m (Path Abs Dir)

backendBinsPath           = relToDirDir absAppDir [binsFolder, backendBinsFolder]
backendDir                = relToDirDir absAppDir [configFolder, supervisorFolder]
configPath                = relToDirDir absAppDir [configFolder]
localLogsDirectory        = relToDirDir absAppDir [logsFolder]
localdataStorageDirectory = relToDirDir absHomeDir [storageDataHomeFolder]
lunaProjectsPath          = relToDirDir getHomeDir [lunaProjects]
packageStudioAtomHome     = relToDirDir absAppDir [userConfigFolder, studioHome]
resourcesDirectory        = relToDirDir absAppDir [binsFolder, resourcesFolder]
sharePath                 = relToDirDir getHomeDir [shareFolder]
userLogsDirectory         = relToDirDir absHomeDir [logsFolder, appName] >>= \p -> fmap (p </>) (parseRelDir =<< version)
userdataStorageDirectory  = relToDirDir absHomeDir [configHomeFolder, appName, storageDataHomeFolder]
windowsLogsDirectory      = relToDirDir absAppDir [configFolder, logsFolder]
windowsScriptsPath        = relToDirDir absAppDir [configFolder, windowsFolder]
userStudioAtomHome = do
    runnerCfg <- get @RunnerConfig
    baseDir   <- relToDirDir absHomeDir [configHomeFolder, appName] >>= \p -> fmap (p </>) (parseRelDir =<< version)
    pure $ baseDir </> (runnerCfg ^. studioHome)
backendLdLibraryPath = do
    ldLibPath <- getCurrentDir
    pure $ ldLibPath </> $(mkRelDir "lib") </> $(mkRelDir "zeromq")

atomAppPath, killSupervisorBinPath, supervisordBinPath :: MonadRun m => m (Path Abs File)
supervisorctlBinPath, versionFilePath, userInfoPath    :: MonadRun m => m (Path Abs File)
atomAppPath               = relToDirFile absAppDir [thirdPartyFolder] atomBinPath
killSupervisorBinPath     = relToDirFile absAppDir [thirdPartyFolder, supervisorKillFolder] supervisorKillBin
supervisordBinPath        = relToDirFile absAppDir [thirdPartyFolder, supervisordFolder] supervisordBin
supervisorctlBinPath      = relToDirFile absAppDir [thirdPartyFolder, supervisordFolder] supervisorctlBin
versionFilePath           = relToDirFile absAppDir [configFolder] versionFile
userInfoPath              = relToDirFile absHomeDir [] userInfoFile

atomHomeDir, logsDir, windowsLogsDir, dataStorageDirectory               :: MonadRun m => Bool -> m (Path Abs Dir)
atomHomeDir          develop = if develop then packageStudioAtomHome     else userStudioAtomHome
logsDir              develop = if develop then localLogsDirectory        else userLogsDirectory
windowsLogsDir       develop = if develop then localLogsDirectory        else windowsLogsDirectory
dataStorageDirectory develop = if develop then localdataStorageDirectory else userdataStorageDirectory
-- misc runner utils --

unixOnly :: MonadRun m => m () -> m ()
unixOnly act = case currentHost of
    Windows -> liftIO $ putStrLn "Current host (Windows) not supported for this operation"
    _       -> act

setEnv :: MonadRun m => String -> String -> m ()
setEnv name val = liftIO $ Environment.setEnv name val

setEnvPath :: MonadRun m => String -> Path b t -> m ()
setEnvPath name val = setEnv name $ toFilePath val

copyLunaStudio :: MonadRun m => m ()
copyLunaStudio = do
    mainHomePath    <- absHomeDir
    packageAtomHome <- packageStudioAtomHome
    atomHomeParent  <- parent <$> userStudioAtomHome
    createDirIfMissing True atomHomeParent
    copyDirRecur packageAtomHome atomHomeParent
    when (currentHost == Windows) $ runProcess_ $ proc "attrib" ["+h", toFilePath mainHomePath]

copyResourcesLinux :: MonadRun m => m ()
copyResourcesLinux = when (currentHost == Linux) $ do
    runnerCfg <- get @RunnerConfig
    versionN  <- T.strip <$> versionText
    resources <- resourcesDirectory
    localShareFolder <- sharePath
    localDesktopFile <- parseRelFile $ "LunaStudio" ++ T.unpack versionN ++ ".desktop"
    let iconsFolder  = resources </> $(mkRelDir "icons")
        desktopFile  = resources </> $(mkRelFile "app_shared.desktop")
        localDesktop = localShareFolder </> $(mkRelDir "applications") </> localDesktopFile
    createDirIfMissing True $ parent localShareFolder
    createDirIfMissing True $ parent localDesktop
    copyDirRecur iconsFolder localShareFolder
    copyFile desktopFile localDesktop

createStorageDataDirectory :: MonadRun m => Bool -> m ()
createStorageDataDirectory develop = do
    dataStoragePath <- dataStorageDirectory develop
    createDirIfMissing True dataStoragePath

checkLunaHome :: MonadRun m => m ()
checkLunaHome = do
    runnerCfg    <- get @RunnerConfig
    userAtomHome <- userStudioAtomHome
    let pathLunaPackage = userAtomHome </> (runnerCfg ^. packageFolder) </> (runnerCfg ^. atomPackageName)
    doesDirExist pathLunaPackage >>= \exists -> unless exists copyLunaStudio

-- supervisord --
supervisorctl :: MonadRun m => [T.Text] -> m T.Text
supervisorctl args = do
    supervisorBinPath <- toFilePath <$> supervisorctlBinPath
    supervisorDir     <- toFilePath <$> backendDir
    let runSupervisorctl = readProcess_ $ setWorkingDir supervisorDir
                                        $ proc supervisorBinPath $ map T.unpack args
        supressErrors act = catchAny (T.decodeUtf8 . BL.toStrict . fst <$> act)
                                     (\_ -> pure "Unable to run supervisorctl")
    liftIO $ supressErrors runSupervisorctl

supervisord :: MonadRun m => Path Rel File -> m ()
supervisord configFile = do
    supervisorBinPath <- toFilePath <$> supervisordBinPath
    supervisorDir     <- toFilePath <$> backendDir
    ldLibPath <- liftIO $ lookupEnv "LD_LIBRARY_PATH"
    setEnv "OLD_LIBPATH" $ fromMaybe "\"\"" ldLibPath
    runProcess_ $ setWorkingDir supervisorDir
                $ shell $ supervisorBinPath ++ " -n -c " ++ toFilePath configFile

stopSupervisor :: MonadRun m => m ()
stopSupervisor = void $ supervisorctl ["shutdown"]

testIfRunning :: MonadRun m => m Bool
testIfRunning = do
    -- TODO[piotrMocz]: we'll need a more robust method eventually
    -- this merely check if there's any luna-related app running
    let lunaApps = Set.fromList ["luna-broker", "luna-ws-connector", "luna-empire", "luna-atom", "luna-undo-redo"] :: Set T.Text
    runningApps <- Set.fromList . T.words <$> supervisorctl ["status", "all"]
    return . not . Set.null $ Set.intersection runningApps lunaApps

-- runner functions --
runLunaEmpire :: MonadRun m => Path Abs Dir -> Path Rel File -> Bool -> m ()
runLunaEmpire logs configFile forceRun = do
    -- NOTE[piotrMocz]: when the `forceRun` flag is set, we will stop any
    -- running instances of supervisord and proceed. If not, they will prevent
    -- the application from running
    running <- testIfRunning
    if running && not forceRun then liftIO $ putStrLn "LunaStudio is already running"
    else do
        when running stopSupervisor
        createDirIfMissing True logs
        supervisord configFile

runFrontend :: MonadRun m => Maybe T.Text -> m ()
runFrontend args = do
    atom <- toFilePath <$> atomAppPath
    createStorageDataDirectory True
    setEnv "LUNA_STUDIO_DEVELOP" "True"
    setEnvPath "ATOM_HOME"             =<< packageStudioAtomHome
    setEnvPath "LUNA_STUDIO_DATA_PATH" =<< dataStorageDirectory True
    setEnvPath "LUNA_PROJECTS"         =<< lunaProjectsPath
    setEnvPath "LUNA_USER_INFO"        =<< userInfoPath
    setEnvPath "LUNA_VERSION_PATH"     =<< versionFilePath
    unixOnly $ runProcess_ $ proc atom $ map T.unpack $ "-w" : maybeToList args

runBackend :: MonadRun m => Bool -> m ()
runBackend forceRun = do
    logs <- localLogsDirectory
    setEnvPath "LUNA_STUDIO_LOG_PATH"     =<< localLogsDirectory
    setEnvPath "LUNA_STUDIO_BACKEND_PATH" =<< backendBinsPath
    setEnvPath "LUNA_STUDIO_CONFIG_PATH"  =<< configPath
    unixOnly $ runLunaEmpire logs $(mkRelFile "supervisord.conf") forceRun

startServices :: MonadRun m => m ()
startServices = case currentHost of
    Windows -> do
        path <- toFilePath <$> windowsScriptsPath
        runProcess_ $ setWorkingDir path $ shell "start.bat"
    _       -> return ()

stopServices :: MonadRun m => m ()
stopServices = case currentHost of
    Windows -> do
        path <- toFilePath <$> windowsScriptsPath
        runProcess_ $ setWorkingDir path $ shell "stop.bat"
    _       -> return ()

runPackage :: MonadRun m => Bool -> Bool -> m ()
runPackage develop forceRun = case currentHost of
    Windows -> do
        atom <- toFilePath <$> atomAppPath
        checkLunaHome
        setEnvPath "LUNA_STUDIO_DATA_PATH" =<< dataStorageDirectory develop
        setEnvPath "LUNA_STUDIO_LOG_PATH"  =<< windowsLogsDir       develop
        setEnvPath "ATOM_HOME"             =<< userStudioAtomHome
        setEnvPath "LUNA_PROJECTS"         =<< lunaProjectsPath
        setEnvPath "LUNA_USER_INFO"        =<< userInfoPath
        setEnvPath "LUNA_VERSION_PATH"     =<< versionFilePath
        createStorageDataDirectory develop
        bracket_ startServices stopServices $ runProcess_ $ shell atom

    _ -> do
        runnerCfg <- get @RunnerConfig
        logs      <- logsDir develop
        let supervisorConf = runnerCfg ^. supervisordConfig
        setEnvPath "LUNA_STUDIO_DATA_PATH"       =<< dataStorageDirectory develop
        setEnvPath "LUNA_STUDIO_GUI_CONFIG_PATH" =<< atomHomeDir          develop
        setEnvPath "LUNA_STUDIO_LOG_PATH"        =<< logsDir              develop
        setEnvPath "LUNA_STUDIO_BACKEND_PATH"    =<< backendBinsPath
        setEnvPath "LUNA_STUDIO_GUI_PATH"        =<< atomAppPath
        setEnvPath "LUNA_STUDIO_CONFIG_PATH"     =<< configPath
        setEnvPath "LUNA_STUDIO_KILL_PATH"       =<< killSupervisorBinPath
        setEnvPath "LUNA_PROJECTS"               =<< lunaProjectsPath
        setEnvPath "LUNA_USER_INFO"              =<< userInfoPath
        setEnvPath "LUNA_VERSION_PATH"           =<< versionFilePath
        when develop $ setEnv "LUNA_STUDIO_DEVELOP" "True"
        if develop then
            setEnv "LUNA_STUDIO_BACKEND_LD_LIBRARY_PATH" "\"\""
            else setEnvPath "LUNA_STUDIO_BACKEND_LD_LIBRARY_PATH" =<< backendLdLibraryPath
        createStorageDataDirectory develop
        unless develop $ do
            checkLunaHome
            copyResourcesLinux
        runLunaEmpire logs supervisorConf forceRun

runApp :: MonadRun m => Bool -> Bool -> Maybe String -> m ()
runApp develop forceRun atom = do
    setEnv "LUNA_STUDIO_ATOM_ARG" (fromMaybe " " atom)
    runPackage develop forceRun

withLunaTempDir :: MonadRun m => m () -> m ()
withLunaTempDir cont = do
    runnerCfg <- get @RunnerConfig
    withSystemTempDir (runnerCfg ^. mainTmpDirectoryTempl) $ \tmpdir -> do
        setEnvPath "LUNA_TMP" tmpdir
        setEnvPath "LUNA_TUTORIALS" $ tmpdir </> (runnerCfg ^. tutorialsDirectory)
        cont

data Options = Options
    { frontend :: Bool
    , backend  :: Bool
    , develop  :: Bool
    , forceRun :: Bool
    , atom     :: Maybe String
    , versioncheck  :: Bool
    } deriving Show

optionParser :: Parser Options
optionParser = Options
    <$> switch (long "frontend"   <> short 'f')
    <*> switch (long "backend"    <> short 'b')
    <*> switch (long "develop"    <> short 'd')
    <*> switch (long "force-run"  <> short 'r')
    <*> optional (strOption $ long "atom" <> short 'a')
    <*> switch (long "version")

run :: Options -> IO ()
run (Options frontend backend develop forceRun atom versionCheck) = do
    hostConfig <- defHostConfig @RunnerConfig
    flip evalStateT hostConfig $
        if versionCheck
        then printVersion
        else withLunaTempDir $
            if frontend
            then runFrontend $ T.pack <$> atom
            else if backend
            then runBackend forceRun
            else runApp develop forceRun atom

filterArg :: String -> Bool
filterArg = not . List.isInfixOf "-psn"

filterArgs :: [String] -> [String]
filterArgs = filter filterArg

filteredParser :: ParserPrefs -> ParserInfo a -> IO a
filteredParser pprefs pinfo = execParserPure pprefs pinfo . filterArgs <$> getArgs >>= handleParseResult

parser :: MonadIO m => m Options
parser = liftIO $ filteredParser p opts
    where
        opts = info (optionParser <**> helper) idm
        p    = prefs showHelpOnEmpty

main :: IO ()
main = run =<< parser
