{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.List              as List
import           GHC.IO.Encoding        (setLocaleEncoding, utf8)

import           Prologue               hiding (switch)
import           System.Log.MLogger
import           System.Log.Options     hiding (info)
import qualified System.Log.Options     as Opt
import qualified ZMQ.Bus.Config         as Config
import qualified ZMQ.Bus.EndPoint       as EP
import           ZMQ.Bus.Logger.Cmd     (Cmd)
import qualified ZMQ.Bus.Logger.Cmd     as Cmd
import qualified ZMQ.Bus.Logger.Logger  as Logger
import qualified ZMQ.Bus.Logger.Version as Version



rootLogger :: Logger
rootLogger = getLogger ""


logger :: Logger
logger = getLogger $moduleName


parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (long "version" <> hidden)
       <|> Cmd.Run
           <$> many      ( strOption ( short 't' <> metavar "TOPIC" <> help "topics to log"))
           <*> optIntFlag (Just "verbose") 'v' 2 3          "Verbose level (level range is 0-5, default level is 3)"
           <*> switch    ( long "no-color"          <> help "Disable color output" )


opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header (Version.full False))


main :: IO ()
main = do
    setLocaleEncoding utf8
    execParser opts >>= run


run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn (Version.full False) -- TODO [PM] hardcoded numeric = False
    Cmd.Run {} -> do
        rootLogger setIntLevel $ Cmd.verbose cmd
        endPoints <- EP.clientFromConfig <$> Config.load
        let topics = if List.null $ Cmd.topics cmd
                        then [""]
                        else Cmd.topics cmd
        Logger.run endPoints topics >>= \case
            Left err -> logger criticalFail err
            _        -> return ()
