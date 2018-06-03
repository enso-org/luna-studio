{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module ZMQ.Bus.WS.Config where

import           Control.Applicative
import qualified Control.Exception   as Exception
import qualified Data.Configurator   as Configurator
import qualified System.Environment  as Env

import           Prologue            hiding (error)
import           System.Log.MLogger



logger :: Logger
logger = getLogger $moduleName


data Config = Config      { websocket :: Section
                          }
            deriving (Show)

data Section = Websocket   { host        :: String
                           , fromWebPort :: String
                           , toWebPort   :: String
                           , pingTime    :: String
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

    cfgFile <- Configurator.load [Configurator.Required $ cpath <> "/config/luna-ws.config"]

    let readConf name = Exception.onException (unsafeFromJustM =<< (Configurator.lookup cfgFile name :: IO (Maybe String)))
                      $ logger error ("Error reading config variable '" <> show name)


    Config <$> ( Websocket <$> readConf "websocket.host"
                           <*> readConf "websocket.fromWebPort"
                           <*> readConf "websocket.toWebPort"
                           <*> readConf "websocket.pingTime"
               )
