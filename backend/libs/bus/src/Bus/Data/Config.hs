module Bus.Data.Config where

import Prologue

import qualified Control.Lens.Aeson         as LensAeson
import qualified Control.Monad.Exception.IO as Exception
import qualified Data.Aeson                 as Aeson
import qualified Data.Yaml                  as Yaml
import qualified Luna.Configurator          as Configurator

import Control.Monad.Exception (Throws)


-- === Definition === --

data Config = Config
    { _pubSocketAddress :: String
    , _subSocketAddress :: String
    } deriving (Generic, Show)

makeLenses ''Config

instance Aeson.ToJSON Config where
    toJSON     = LensAeson.toJSON
    toEncoding = LensAeson.toEncoding

instance Aeson.FromJSON Config where
    parseJSON = LensAeson.parse


-- === File API === --

readFromFile ::
    (Throws Yaml.ParseException m, MonadIO m) => FilePath -> m Config
readFromFile =
    liftIO . Exception.rethrowFromIO @Yaml.ParseException . Yaml.decodeFileThrow

readDefault :: (Throws Yaml.ParseException m, MonadIO m) => m Config
readDefault = readFromFile =<< Configurator.busConfigPath
