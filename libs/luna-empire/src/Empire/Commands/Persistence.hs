{-# LANGUAGE OverloadedStrings #-}

module Empire.Commands.Persistence () where

-- import           Control.Monad.Except                (throwError)
-- import           Control.Monad.Reader
-- import           Control.Monad.State
-- import qualified Data.ByteString.Lazy                as BS (ByteString, readFile, writeFile)
-- import qualified Data.IntMap                         as IntMap
-- import           Data.String                         (fromString)
-- import           Data.Text                           (Text)
-- import qualified Data.UUID                           as UUID
-- import           Empire.Prelude
-- import           System.Environment                  (getEnv)
-- import           System.FilePath                     (takeBaseName, (</>))

-- import qualified Empire.Data.Library                 as Library
-- import           Empire.Data.Project                 (Project)
-- import qualified Empire.Data.Project                 as Project

-- import qualified LunaStudio.API.Persistence.Envelope as E
-- import qualified LunaStudio.API.Persistence.Library  as L
-- import qualified LunaStudio.API.Persistence.Project  as P
-- import qualified LunaStudio.Data.Graph               as G
-- import           LunaStudio.Data.GraphLocation       (GraphLocation (..))
-- import           LunaStudio.Data.PortRef             (AnyPortRef (InPortRef'))
-- import           LunaStudio.Data.Project             (ProjectId)
-- import qualified Luna.Project                        as Project

-- import           Empire.ASTOp                        (runASTOp)
-- import qualified Empire.Commands.Graph               as Graph
-- import           Empire.Commands.GraphBuilder        (buildGraph)
-- import           Empire.Commands.Library             (createLibrary, listLibraries, withLibrary)
-- import           Empire.Empire                       (Empire)

-- import qualified Data.Aeson                          as JSON
-- import qualified Data.Aeson.Encode.Pretty            as JSON

-- import qualified System.Log.MLogger                  as Logger

-- logger :: Logger.Logger
-- logger = Logger.getLogger $(Logger.moduleName)

-- defaultLibraryName, defaultLibraryPath :: String
-- defaultLibraryName = "Main"
-- defaultLibraryPath = "Main.luna"

-- touch :: FilePath -> IO ()
-- touch name = appendFile name ""

-- createDefaultProject :: Empire ()
-- createDefaultProject = do
--   logger Logger.info "Creating default project"
--   lunaroot <- liftIO $ getEnv Project.lunaRootEnv
--   let path = lunaroot </> "projects" </> defaultLibraryPath
--   logger Logger.info $ "Creating file " <> path
--   liftIO $ touch path
--   void $ createLibrary (Just defaultLibraryName) (fromString path)
