module Main where

import           GHC.IO.Encoding        (setLocaleEncoding, utf8)
import           Prologue
import           System.Log.MLogger
import           System.Log.Options
import qualified System.Log.Options      as Opt
import           WSConnector.Cmd         (Cmd (..))
import qualified WSConnector.Version     as Version
import qualified WSConnector.WSConfig    as WSConfig
import qualified WSConnector.WSConnector as WSConnector
import qualified ZMQ.Bus.Config          as Config
import qualified ZMQ.Bus.EndPoint        as EndPoint
import qualified ZMQ.Bus.WS.Config       as WSConfigLoader

import System.Remote.Monitoring (forkServer)


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
    -- forkServer "127.0.0.1" 12345
    execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Version     -> putStrLn (Version.full False)
    Run verbosity -> do
        busEndPoints <- EndPoint.clientFromConfig <$> Config.load
        config <- WSConfig.readWebsocketConfig <$> WSConfigLoader.load
        rootLogger setIntLevel verbosity
        WSConnector.run busEndPoints config
