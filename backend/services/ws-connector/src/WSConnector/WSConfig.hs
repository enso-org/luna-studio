{-# LANGUAGE TemplateHaskell #-}

module WSConnector.WSConfig where

import           Prologue

import qualified Control.Lens.Aeson         as LensAeson
import qualified Control.Monad.Exception.IO as Exception
import qualified Data.Aeson                 as Aeson
import qualified Data.Yaml                  as Yaml
import qualified Luna.Configurator          as Configurator

import Control.Monad.Exception (Throws)

data Config = Config
    { _host        :: String
    , _fromWebPort :: Int
    , _toWebPort   :: Int
    , _pingTime    :: Int
    } deriving (Generic, Read, Show, Eq)

makeLenses ''Config

instance Aeson.ToJSON Config where
    toJSON     = LensAeson.toJSON
    toEncoding = LensAeson.toEncoding

instance Aeson.FromJSON Config where
    parseJSON = LensAeson.parse

readFromFile ::
    (Throws Yaml.ParseException m, MonadIO m) => FilePath -> m Config
readFromFile =
    liftIO . Exception.rethrowFromIO @Yaml.ParseException . Yaml.decodeFileThrow

readDefault :: (Throws Yaml.ParseException m, MonadIO m) => m Config
readDefault = readFromFile =<< Configurator.websocketConfigPath
