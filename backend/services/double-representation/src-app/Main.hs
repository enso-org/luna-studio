{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prologue

import qualified Bus.Data.Config    as Config
import qualified Data.List          as List
import qualified Empire.Cmd         as Cmd
import qualified Empire.Server      as Server
import qualified Empire.Version     as Version
import qualified System.Log.Options as Opt

import Empire.Cmd         (Cmd)
import GHC.IO.Encoding    (setLocaleEncoding, utf8)
import System.Log.MLogger (Logger, getLogger, moduleName, setIntLevel)
import System.Log.Options (help, long, metavar, short)

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
           <*> (not <$> Opt.switch (long "unformatted" <> help "Unformatted output" ))

opts :: Opt.ParserInfo Cmd
opts = Opt.info (Opt.helper <*> parser)
                (Opt.fullDesc <> Opt.header Version.fullVersion)

main :: IO ()
main = do
    setLocaleEncoding utf8
    Opt.execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn Version.fullVersion
    Cmd.Run {} -> do
        rootLogger setIntLevel $ Cmd.verbose cmd
        endPoints <- Config.readDefault
        let topics = if List.null $ Cmd.topics cmd
                        then defaultTopics
                        else Cmd.topics cmd
        Server.run endPoints topics
