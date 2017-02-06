{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.List          as List
import           Prologue

import           Empire.Cmd         (Cmd)
import qualified Empire.Cmd         as Cmd
import qualified Empire.Server      as Server
import qualified Empire.Version     as Version
import           System.Log.MLogger
import           System.Log.Options (help, long, metavar, short)
import qualified System.Log.Options as Opt
import qualified ZMQ.Bus.Config     as Config
import qualified ZMQ.Bus.EndPoint   as EP

defaultTopics :: [String]
defaultTopics = ["empire."]

rootLogger :: Logger
rootLogger = getLogger ""

logger :: Logger
logger = getLogger $moduleName

parser :: Opt.Parser Cmd
parser = Opt.flag' Cmd.Version (short 'V' <> long "version" <> help "Version information")
       <|> Cmd.Run
           <$> Opt.many         (Opt.strOption (short 't' <> metavar "TOPIC" <> help "Topic to listen"))
           <*> Opt.optIntFlag   (Just "verbose") 'v' 2 3 "Verbosity level (0-5, default 3)"
           <*> not . Opt.switch (long "unformatted" <> help "Unformatted output" )

opts :: Opt.ParserInfo Cmd
opts = Opt.info (Opt.helper <*> parser)
                (Opt.fullDesc <> Opt.header Version.fullVersion)

main :: IO ()
main = Opt.execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn Version.fullVersion
    Cmd.Run {} -> do
        rootLogger setIntLevel $ Cmd.verbose cmd
        endPoints <- EP.clientFromConfig <$> Config.load
        projectRoot <- Config.projectRoot <$> Config.projects <$> Config.load
        let topics = if List.null $ Cmd.topics cmd
                        then defaultTopics
                        else Cmd.topics cmd
            formatted = Cmd.formatted cmd
        r <- Server.run endPoints topics formatted projectRoot
        case r of
            Left err -> logger criticalFail err
            _        -> return ()
