{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module ZMQ.Bus.Config where

import           Control.Applicative
import qualified Control.Exception   as Exception
import qualified Data.Configurator   as Configurator
import qualified System.Environment  as Env

import           Prologue            hiding (error)
import           System.Log.MLogger



logger :: Logger
logger = getLogger $moduleName


data Config = Config      { root      :: Section
                          , projects  :: Section
                          , bus       :: Section
                          }
            deriving (Show)

data Section = Root       { path :: String
                          , conf :: String
                          , bin  :: String
                          }
             | Projects   { projectRoot  :: String
                          }
             | Bus        { serverControlEndPoint :: String
                          , serverPullEndPoint    :: String
                          , serverPubEndPoint     :: String
                          , clientControlEndPoint :: String
                          , clientPullEndPoint    :: String
                          , clientPubEndPoint     :: String
                          }
             deriving (Show)


lunaRootEnv :: String
lunaRootEnv = "LUNA_LIBS_PATH"


load :: IO Config
load = do
    logger debug "Loading Luna configuration"
    cpath <- Exception.onException (Env.getEnv lunaRootEnv)
           $ logger error ("Luna environment not initialized.")
          *> logger error ("Environment variable '" <> lunaRootEnv <> "' not defined.")
          *> logger error ("Please run 'source <LUNA_INSTALL_PATH>/setup' and try again.")

    cfgFile <- Configurator.load [Configurator.Required $ cpath <> "/config/luna.config"]

    let readConf name = Exception.onException (unsafeFromJustM =<< (Configurator.lookup cfgFile name :: IO (Maybe String)))
                      $ logger error ("Error reading config variable '" <> show name)

    --let readConfDefault val name = Configurator.lookupDefault val cfgFile name

    Config <$> ( Root <$> readConf "root.path"
                      <*> readConf "root.conf"
                      <*> readConf "root.bin"
               )
           <*> ( Projects <$> readConf "projects.projectRoot"
               )
           <*> ( Bus      <$> readConf "bus.serverControlEndPoint"
                          <*> readConf "bus.serverPullEndPoint"
                          <*> readConf "bus.serverPubEndPoint"
                          <*> readConf "bus.clientControlEndPoint"
                          <*> readConf "bus.clientPullEndPoint"
                          <*> readConf "bus.clientPubEndPoint"
               )

-- TODO[wd]: (?) Lunac powinien czytac config i jezli nie da sie go odczytac (np zmienna srodowiskowa nie istnieje, powinien zalozyc, ze zyje w $HOME/.luna - defaultowy config?)
