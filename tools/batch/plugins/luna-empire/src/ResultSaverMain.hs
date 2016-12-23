{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Aeson                     as JSON
import qualified Data.Aeson.Encode.Pretty       as JSON
import qualified Data.Binary                    as Bin
import qualified Data.ByteString.Lazy           as BS
import qualified Data.List                      as List
import           Data.Maybe                     (fromMaybe)
import qualified Data.UUID                      as UUID
import           Prologue

import           Empire.API.Data.Breadcrumb     (Breadcrumb (..))
import           Empire.API.Data.GraphLocation  (GraphLocation (..))
import qualified Empire.ResultSaver             as ResultSaver
import           Empire.ResultSaver.Cmd         (Cmd)
import qualified Empire.ResultSaver.Cmd         as Cmd
import           Empire.ResultSaver.ProjectDump (ProjectDump)
import qualified Empire.Version                 as Version
import           System.Log.MLogger
import           System.Log.Options             (help, long, metavar, optional, short)
import qualified System.Log.Options             as Opt
import qualified ZMQ.Bus.Config                 as Config
import qualified ZMQ.Bus.EndPoint               as EP

defaultTopic :: String
defaultTopic = "empire."

logger :: Logger
logger = getLogger $moduleName

parser :: Opt.Parser Cmd
parser =   Opt.flag' Cmd.Version (short 'V' <> long "version" <> help "Version information")
       <|> Cmd.Save          <$>             Opt.strOption ( long "project"     <> metavar "UUID" <> help "Project to dump" )
                             <*> (optional $ Opt.strOption ( long "out"         <> metavar "FILE" <> help "Output filename" ))
       <|> Cmd.ImportAndSave <$>             Opt.strOption ( long "projectFile" <> metavar "FILE" <> help "Project import" )
                             <*>             Opt.strOption ( long "out"         <> metavar "FILE" <> help "Output filename" )

opts :: Opt.ParserInfo Cmd
opts = Opt.info (Opt.helper <*> parser)
                (Opt.fullDesc <> Opt.header Version.fullVersion)

main :: IO ()
main = Opt.execParser opts >>= run

instance JSON.ToJSON ProjectDump

run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn Version.fullVersion
    Cmd.Save projectIdStr outFile -> do
      endPoints <- EP.clientFromConfig <$> Config.load
      let projectId = fromMaybe (UUID.nil) (UUID.fromString projectIdStr)
          gl = GraphLocation projectId 0 (Breadcrumb [])
      r <- ResultSaver.save endPoints gl
      case r of
          Left err   -> logger criticalFail err
          Right dump -> do
            let outputName = fromMaybe (projectIdStr <> ".json") outFile
            let binData = JSON.encodePretty dump
            BS.writeFile outputName binData
    Cmd.ImportAndSave inFile outFile -> do
      endPoints <- EP.clientFromConfig <$> Config.load
      projectData <- readFile inFile
      r <- ResultSaver.importAndSave endPoints (convert projectData)
      case r of
          Left err   -> logger criticalFail err
          Right dump -> do
            let binData = JSON.encodePretty dump
            BS.writeFile outFile binData
