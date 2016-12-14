{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Prologue

import           Cmd         (Cmd)
import qualified Cmd         as Cmd
import           Version

import qualified System.Log.Options   as Opt
import           System.Log.Options (help, long, metavar, short)
import qualified Undo                 as Undo
import qualified ZMQ.Bus.EndPoint     as EP
import qualified ZMQ.Bus.Config       as Config

parser :: Opt.Parser Cmd
parser = Opt.flag' Cmd.Version (short 'V' <> long "version" <> help "Version information")
       <|> Cmd.Run
           <$> Opt.many         (Opt.strOption (short 't' <> metavar "TOPIC" <> help "Topic to listen"))
           <*> Opt.optIntFlag   (Just "verbose") 'v' 2 4 "Verbosity level (0-5, default 3)"
           <*> not . Opt.switch (long "unformatted" <> help "Unformatted output" )

opts :: Opt.ParserInfo Cmd
opts = Opt.info (Opt.helper <*> parser)
                (Opt.fullDesc <> Opt.header Version.fullVersion)

main :: IO ()
main = Opt.execParser opts >>= run

run :: Cmd -> IO()
run cmd = case cmd of
    Cmd.Version  -> putStrLn Version.fullVersion
    Cmd.Run {} -> do
        endPoints <- EP.clientFromConfig <$> Config.load
        Undo.runUndo endPoints Undo.empty
