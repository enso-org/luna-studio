module Luna.Configurator where

import Prologue

import qualified System.Environment as Env

import System.FilePath ((</>))


-- === API === --

envVarName :: String
envVarName = "LUNA_LIBS_PATH"

envPath :: MonadIO m => m FilePath
envPath = liftIO $ Env.getEnv envVarName

configDirName :: String
configDirName = "config"

configDirPath :: MonadIO m => m FilePath
configDirPath = (</> configDirName) <$> envPath

busConfigFileName :: String
busConfigFileName = "bus.yaml"

busConfigPath :: MonadIO m => m FilePath
busConfigPath = (</> busConfigFileName) <$> configDirPath

websocketConfigFileName :: String
websocketConfigFileName = "websocket.yaml"

websocketConfigPath :: MonadIO m => m FilePath
websocketConfigPath = (</> websocketConfigFileName) <$> configDirPath
