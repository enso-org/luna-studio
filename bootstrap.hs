{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
import qualified Shelly.Lifted as Shelly
import Shelly.Lifted (MonadSh, (</>), shelly, liftIO)
import qualified System.Directory as System
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad (when, unless)
import Control.Monad.IO.Class ( MonadIO)
import Control.Exception (Exception)
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.IO (writeFile)
import Data.Monoid ((<>))
import Filesystem.Path.CurrentOS (parent, encodeString, fromText)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import qualified System.Directory as System
default (T.Text)

stack = "../../tools/stack/stack"

tools = "../../tools"
libs = "../../libs"
supportedNodeVersion = "6.11.3"
supportedPythonVersion = "3.6.2"
lunaShell = "./luna-shell.sh"

currentPath :: (MonadSh m, MonadIO m) => m Text
currentPath = do
    path <- Shelly.get_env "APP_PATH"
    currentDirectory <- liftIO $ System.getCurrentDirectory
    return $ fromMaybe (T.pack currentDirectory) path

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

sanityCheck :: (MonadSh m, Shelly.MonadShControl m) => Shelly.FilePath -> [T.Text] -> m ()
sanityCheck command params = Shelly.silently $ do
    Shelly.errExit False $ bashLogin command params
    exit <- Shelly.lastExitCode
    when (exit /= 0) $ Shelly.errorExit (Shelly.toTextIgnore command)

installPython :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
installPython = do
    Shelly.echo "installing python locally"
    current <- currentPath
    let pythonFolder   = current </> tools </> "python"
    Shelly.chdir_p pythonFolder $ do
        pyenvPresent <- Shelly.test_d "pyenv"
        unless pyenvPresent $ Shelly.cmd "git" "clone" "https://github.com/pyenv/pyenv.git"

        Shelly.setenv "PYENV_ROOT" $ Shelly.toTextIgnore $ pythonFolder </> "pyenv"
        Shelly.prependToPath $ pythonFolder </> "pyenv" </> "bin"
        Shelly.prependToPath $ pythonFolder </> "pyenv" </> "shims"
        Shelly.cmd "pyenv" "init" "-"

        pythonSuppertedVersionPresent <- Shelly.test_d $ "pyenv/versions" </> supportedPythonVersion
        unless pythonSuppertedVersionPresent $ do
            -- we need this because of https://github.com/pyenv/pyenv/issues/950
            when (currentHost == Darwin) $ do
                opensslPath <- T.stripEnd <$> Shelly.cmd "brew" "--prefix" "openssl"
                Shelly.setenv "CFLAGS"  $ "-I" <> opensslPath <> "/include"
                Shelly.setenv "LDFLAGS" $ "-L" <> opensslPath <> "/lib"
            Shelly.cmd "pyenv" "install" supportedPythonVersion
        Shelly.cmd "pyenv" "local" supportedPythonVersion
        Shelly.chdir (fromText current) $ Shelly.cmd "pip" "install" "--user" "-r" "requirements.txt"

installNode :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
installNode = do
    Shelly.echo "installing node locally"
    current <- currentPath
    let nodeFolder = current </> tools </> "node"
        arch       = if currentHost == Darwin then "darwin" else "linux" :: Text
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

installNodeModules :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
installNodeModules = do
    Shelly.echo "installing node modules"
    current <- currentPath
    let nodeBinPath = current </> tools </> "node" </> supportedNodeVersion </> "bin"
    Shelly.prependToPath nodeBinPath
    Shelly.cmd (nodeBinPath </> "npm") $ "install" : nodeModules


haskellBins :: [T.Text]
haskellBins = ["happy", "hsc2hs"]

installHaskellBins :: (MonadSh m, Shelly.MonadShControl m, MonadIO m) => m ()
installHaskellBins = do
    current <- currentPath
    home <- liftIO $ System.getHomeDirectory
    Shelly.appendToPath $ home </> ".local/bin"
    mapM (Shelly.cmd (current </> stack) "--resolver" "lts-8.2" "install" "--install-ghc") haskellBins
    sanityCheck "happy" ["--version"]
    sanityCheck "hsc2hs" ["--version"]

downloadLibs :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
downloadLibs = do
    Shelly.echo "downloading libraries"
    current <- currentPath
    let libsFolder    = current </> libs
        arch          = if currentHost == Darwin then "darwin" else "linux"
        lunaStudioUrl = "https://s3-us-west-2.amazonaws.com/packages-luna/" <> arch <> "/libs/luna-studio-libs.tar.gz"
    Shelly.chdir_p (parent libsFolder) $ do
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

getStackPaths :: (MonadSh m, MonadIO m, Shelly.MonadShControl m) => m Text
getStackPaths = do
    current <- currentPath
    let absStackPath = current </> stack
    Shelly.silently $ Shelly.run absStackPath ["path", "--bin-path"]

generateLunaShellScript :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
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
        lunaShellPath      = current </> lunaShell
        fullCode           = T.unlines [addLdLibraryPath, addPath, pyenvEnviromentVar, loadPython]
    liftIO $ Data.Text.IO.writeFile (encodeString lunaShellPath) fullCode

stackSetupForLunaStudio :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
stackSetupForLunaStudio = do
    current <- currentPath
    Shelly.chdir (current </>"luna-studio") $ do
        Shelly.echo "install GHCJS"
        Shelly.cmd (current </>stack) "setup"


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
        installHaskellBins
