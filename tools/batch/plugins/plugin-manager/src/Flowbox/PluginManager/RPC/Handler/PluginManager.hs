---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.RPC.Handler.PluginManager where

import           Flowbox.Prelude                                   hiding (error, id)
import           Flowbox.System.Log.Logger
import qualified Generated.Proto.PluginManager.Plugin.Ping.Request as Ping
import qualified Generated.Proto.PluginManager.Plugin.Ping.Status  as Ping



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.RPC.Handler.Plugin"

-------- public api -------------------------------------------------

ping :: Ping.Request -> IO Ping.Status
ping Ping.Request = do
    logger info "Ping received"
    return Ping.Status
