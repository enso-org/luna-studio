{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
import qualified Shelly.Lifted as Shelly
import Shelly.Lifted (MonadSh, MonadShControl, (</>), shelly, liftIO)
import qualified System.Directory as System
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class ( MonadIO)
import Control.Exception (Exception)
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.IO (writeFile)
import Data.Monoid ((<>))
import Filesystem.Path.CurrentOS (parent, encodeString)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import qualified System.Directory as System
default (T.Text)


-- === Paths === --

stack = "tools/stack/stack"
tools = "tools"
libs = "libs"
supportedNodeVersion = "6.11.3"
supportedPythonVersion = "3.6.2"
lunaShell = "./luna-shell.sh"

pyenvRepoUrl :: Text
pyenvRepoUrl = "https://github.com/pyenv/pyenv.git"

currentPath :: (MonadSh m, MonadIO m) => m Shelly.FilePath
currentPath = do
    path <- Shelly.get_env "APP_PATH"
    currentDirectory <- Shelly.pwd
    return $ fromMaybe currentDirectory (Shelly.fromText <$> path)

toolsPath, pythonPath, pyenvPath, nodePath, appsPath, studioPath, lunaShellPath :: (MonadSh m, MonadIO m) => m Shelly.FilePath
toolsPath     = (</> "tools")       <$> currentPath
pythonPath    = (</> "python")      <$> toolsPath
pyenvPath     = (</> "pyenv")       <$> pythonPath
nodePath      = (</> "node")        <$> toolsPath
appsPath      = (</> "apps")        <$> currentPath
studioPath    = (</> "luna-studio") <$> appsPath
lunaShellPath = (</> lunaShell)     <$> studioPath

chdir_pM, chdirM :: MonadShControl m => m Shelly.FilePath -> m a -> m ()
chdir_pM pM act = pM >>= \p -> void $ Shelly.chdir_p p act
chdirM   pM act = pM >>= \p -> void $ Shelly.chdir   p act


-------------------
-- === Hosts === --
-------------------

-- === Definition === --

data System = Linux
            | Darwin
            | Windows
            deriving (Show, Eq)


-- === System discovery === --

currentHost :: System

#ifdef linux_HOST_OS
currentHost      =  Linux
#elif darwin_HOST_OS
currentHost      =  Darwin
#elif mingw32_HOST_OS
currentHost      =  Windows
#else
Running on unsupported system.
#endif

sanityCheck :: (MonadSh m, MonadShControl m) => Shelly.FilePath -> [T.Text] -> m ()
sanityCheck command params = Shelly.silently $ do
    Shelly.errExit False $ bashLogin command params
    exit <- Shelly.lastExitCode
    when (exit /= 0) $ Shelly.errorExit (Shelly.toTextIgnore command)


-- === Installing Python === --

installPython :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
installPython = do
    Shelly.echo "Installing python locally..."
    _setupPyenv
    _ensurePythonVersionInstalled
    _installPythonDeps

_setupPyenv :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
_setupPyenv = chdir_pM pythonPath $ do
        pyenvPresent <- Shelly.test_d "pyenv"
        unless pyenvPresent $ Shelly.cmd "git" "clone" pyenvRepoUrl

        pyenvPath' <- pyenvPath
        Shelly.setenv "PYENV_ROOT" $ Shelly.toTextIgnore $ pyenvPath'
        Shelly.prependToPath $ pyenvPath' </> "bin"
        Shelly.prependToPath $ pyenvPath' </> "shims"
        Shelly.cmd "pyenv" "init" "-"

_ensurePythonVersionInstalled :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
_ensurePythonVersionInstalled = do
    pyenvPath' <- pyenvPath
    let versionPath = pyenvPath' </> "versions" </> supportedPythonVersion
    pythonSupportedVersionPresent <- Shelly.test_d versionPath

    chdirM currentPath $ do
        unless pythonSupportedVersionPresent $ do
            -- we need this because of https://github.com/pyenv/pyenv/issues/950
            when (currentHost == Darwin) $ do
                opensslPath <- T.stripEnd <$> Shelly.cmd "brew" "--prefix" "openssl"
                Shelly.setenv "CFLAGS"  $ "-I" <> opensslPath <> "/include"
                Shelly.setenv "LDFLAGS" $ "-L" <> opensslPath <> "/lib"
            Shelly.cmd "pyenv" "install" supportedPythonVersion
    
    chdirM studioPath $
        Shelly.cmd "pyenv" "local" supportedPythonVersion

_installPythonDeps :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
_installPythonDeps = chdirM studioPath $
    Shelly.cmd "pip" "install" "--user" "-r" "requirements.txt"


-- === Installing Node === --

installNode :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
installNode = do
    Shelly.echo "installing node locally"
    nodeFolder <- nodePath
    let arch       = if currentHost == Darwin then "darwin" else "linux" :: Text
        nodeVer    = Shelly.toTextIgnore supportedNodeVersion
        nodeTar    = "node-v" <> nodeVer <> "-" <> arch <> "-x64.tar.gz"
        nodeUrl    = "https://nodejs.org/dist/v" <> nodeVer <> "/" <> nodeTar
    Shelly.echo $ "NodeTar: " <> nodeTar
    Shelly.echo $ "NodeUrl: " <> nodeUrl
    Shelly.chdir_p nodeFolder $ do
        Shelly.mkdir_p supportedNodeVersion
        Shelly.cmd "wget" nodeUrl
        Shelly.cmd "tar" "-xpzf" nodeTar "--strip=1" "-C" supportedNodeVersion
        Shelly.rm $ Shelly.fromText nodeTar

nodeModules :: [T.Text]
nodeModules = ["less"]

installNodeModules :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
installNodeModules = do
    Shelly.echo "installing node modules"
    nodeFolder <- nodePath
    let nodeBinPath = nodeFolder </> supportedNodeVersion </> "bin"
    Shelly.prependToPath nodeBinPath
    Shelly.cmd (nodeBinPath </> "npm") $ "install" : nodeModules


-- === Installing Haskell === --

stackSetupForLunaStudio :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
stackSetupForLunaStudio = do
    current <- currentPath
    chdirM studioPath $ do
        Shelly.echo "install GHCJS"
        Shelly.cmd (current </> stack) "setup"


-- === Other utils === --

downloadLibs :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
downloadLibs = do
    Shelly.echo "downloading libraries"
    current <- currentPath
    let arch          = if currentHost == Darwin then "darwin" else "linux"
        lunaStudioUrl = "http://packages.luna-lang.org/" <> arch <> "/libs/luna-studio-libs.tar.gz"
    Shelly.chdir_p current $ do
        Shelly.cmd "wget" lunaStudioUrl
        Shelly.cmd  "tar" "-xpzf" "./luna-studio-libs.tar.gz" "--strip=1"
        Shelly.rm "./luna-studio-libs.tar.gz"

bashLogin :: MonadSh m => Shelly.FilePath -> [T.Text] -> m T.Text
bashLogin command params = do
    Shelly.cmd "bash" ["-c", "-l", (Shelly.toTextIgnore command) `T.append` " " `T.append` T.intercalate " " params]

checkShell :: MonadSh m => m Text
checkShell = fromMaybe "bash" <$> Shelly.get_env "SHELL"

preparePaths :: [Shelly.FilePath] -> Text
preparePaths filepaths = intercalate ":" $ Shelly.toTextIgnore <$> filepaths

getStackPaths :: (MonadSh m, MonadIO m, MonadShControl m) => m Text
getStackPaths = do
    current <- currentPath
    let absStackPath = current </> stack
    Shelly.silently $ Shelly.run absStackPath ["path", "--bin-path"]

generateLunaShellScript :: (MonadIO m, MonadSh m, MonadShControl m) => m ()
generateLunaShellScript = do
    Shelly.echo "generate luna shell"
    current <- currentPath
    stackPaths <- getStackPaths
    let lbsPath            = current </> libs
        pyenvEnviromentVar = "export PYENV_ROOT=" <> (Shelly.toTextIgnore $ current </> tools </> "python" </> "pyenv")
        pyenvShimsFolder   = "${PYENV_ROOT}" </> "shims"
        pyenvBinFolder     = "${PYENV_ROOT}" </> "bin"
        stackPath          = current </> (parent stack)
        nodeBinPath        = current </> tools </> "node" </> supportedNodeVersion </> "bin"
        addLdLibraryPath   = "export LD_LIBRARY_PATH=" <> Shelly.toTextIgnore lbsPath <> ":${LD_LIBRARY_PATH}"
        paths              = preparePaths [stackPath, pyenvShimsFolder, pyenvBinFolder, nodeBinPath]
        addPath            = "export PATH=" <> paths <> ":" <> T.strip stackPaths <> ":$PATH"
        initPyenv          = "eval \"$(pyenv init -)\""
        loadPython         = "pyenv" <>  " local " <> Shelly.toTextIgnore supportedPythonVersion
        fullCode           = T.unlines [addLdLibraryPath, addPath, pyenvEnviromentVar, initPyenv, loadPython]

    lunaShellP <- lunaShellPath
    liftIO $ Data.Text.IO.writeFile (encodeString lunaShellP) fullCode


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    shelly $ do
        installPython
        installNode
        installNodeModules
        downloadLibs
        generateLunaShellScript
        stackSetupForLunaStudio
