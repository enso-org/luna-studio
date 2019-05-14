module Main where

import           Prologue

import qualified Bus.Data.Config         as Config
import qualified System.Log.Options      as Opt
import qualified WSConnector.Version     as Version
import qualified WSConnector.WSConfig    as WSConfig
import qualified WSConnector.WSConnector as WSConnector

import GHC.IO.Encoding          (setLocaleEncoding, utf8)
import System.Log.MLogger
import System.Log.Options
import System.Remote.Monitoring (forkServer)
import WSConnector.Cmd          (Cmd (..))


rootLogger :: Logger
rootLogger = getLogger ""

parser :: Parser Cmd
parser = Opt.flag' Version (long "version" <> hidden)
       <|> Run <$> optIntFlag (Just "verbose") 'v' 2 3 "Verbose level (level range is 0-5, default is 3)"

opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header (Version.full False))

main :: IO ()
main = do
    setLocaleEncoding utf8
    execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Version     -> putStrLn (Version.full False)
    Run verbosity -> do
        busConfig <- Config.readDefault
        wsConfig  <- WSConfig.readDefault
        rootLogger setIntLevel verbosity
        WSConnector.run busConfig wsConfig
