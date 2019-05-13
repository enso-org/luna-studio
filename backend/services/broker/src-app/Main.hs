{-# LANGUAGE TemplateHaskell #-}

module Main where


import Prologue

import qualified Bus.Data.Config        as Config
import qualified Bus.Framework.App      as Bus
import qualified GHC.IO.Encoding        as Encoding
import qualified System.Log.MLogger     as Logger
import qualified System.Log.Options     as Opt
import qualified ZMQ.Bus.Broker.Cmd     as Cmd
import qualified ZMQ.Bus.Broker.Version as Version

import ZMQ.Bus.Broker.Cmd (Cmd)

rootLogger :: Logger.Logger
rootLogger = Logger.getLogger ""

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

parser :: Opt.Parser Cmd
parser = Opt.flag' Cmd.Version (Opt.long "version" <> Opt.hidden)
    <|> Cmd.Serve
        <$> Opt.optIntFlag (Just "verbose") 'v' 2 3
            "Verbose level (level range is 0-5, default level is 3)"
        <*> Opt.switch (Opt.long "no-color" <> Opt.help "Disable color output")

opts :: Opt.ParserInfo Cmd
opts = Opt.info (Opt.helper <*> parser)
    (Opt.fullDesc <> Opt.header (Version.full False))

main :: IO ()
main = do
    Encoding.setLocaleEncoding Encoding.utf8
    Opt.execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn (Version.full False)
    Cmd.Serve {} -> do
        rootLogger Logger.setIntLevel $ Cmd.verbose cmd
        logger Logger.info "Starting proxy service"
        config <- Config.readDefault
        Bus.runProxy config
