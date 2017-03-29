module Empire.Commands.Persistence
    ( saveProject
    , saveLocation
    , loadProject
    , createDefaultProject
    , importProject
    , exportProject
    ) where

import           Control.Monad.Except            (throwError)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy            as BS (ByteString, readFile, writeFile)
import qualified Data.IntMap                     as IntMap
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import qualified Data.UUID                       as UUID
import           Empire.Prelude
import           System.FilePath                 (takeBaseName)

import qualified Empire.Data.Library             as Library
import           Empire.Data.Project             (Project)
import qualified Empire.Data.Project             as Project

import qualified Empire.API.Data.Graph           as G
import           Empire.API.Data.GraphLocation   (GraphLocation (..))
import           Empire.API.Data.PortRef         (AnyPortRef(InPortRef'))
import           Empire.API.Data.Project         (ProjectId)
import qualified Empire.API.Persistence.Envelope as E
import qualified Empire.API.Persistence.Library  as L
import qualified Empire.API.Persistence.Project  as P

import           Empire.ASTOp                    (runASTOp)
import qualified Empire.Commands.Graph           as Graph
import           Empire.Commands.GraphBuilder    (buildGraph)
import           Empire.Commands.Library         (createLibrary, listLibraries, withLibrary)
import           Empire.Empire                   (Empire)

import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Encode.Pretty        as JSON

import           Empire.API.JSONInstances        ()

import qualified System.Log.MLogger              as Logger

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)


toPersistentProject :: ProjectId -> Empire P.Project
toPersistentProject pid = $notImplemented -- do
  -- libs <- listLibraries pid
  -- almostProject <- withProject pid $ do
  --   proj <- get
  --   return $ Project.toPersistent proj
  --
  -- libs' <- forM (libs) $ \(lid, lib) -> do
  --   graph <- withLibrary pid lid . zoom Library.body $ runASTOp buildGraph
  --   return $ (lid, Library.toPersistent lib graph)
  --
  -- return $ almostProject $ IntMap.fromList libs'

serialize :: E.Envelope -> BS.ByteString
serialize = JSON.encodePretty

saveProject :: FilePath -> ProjectId -> Empire ()
saveProject projectRoot pid = do
  project <- toPersistentProject pid
  let bytes = serialize $ E.pack project
      path  = projectRoot <> "/" <> (UUID.toString pid) <> ".lproj"

  logger Logger.info $ "Saving project " <> path
  liftIO $ BS.writeFile path bytes


saveLocation :: FilePath -> GraphLocation -> Empire ()
saveLocation projectRoot (GraphLocation _ _) = return () -- saveProject projectRoot pid

readProject :: BS.ByteString -> Maybe P.Project
readProject bytes = (view E.project) <$> envelope where
  envelope = JSON.decode bytes
  -- TODO: migrate data



createProjectFromPersistent :: Maybe ProjectId -> P.Project -> Empire (ProjectId, Project)
createProjectFromPersistent maybePid p = $notImplemented -- do
  -- (pid, _) <- createProject maybePid (p ^. P.name)
  --
  -- forM_ (p ^. P.libs) $ \lib -> do
  --   (lid, _) <- createLibrary pid (lib ^. L.name) (fromString $ lib ^. L.path)
  --   withLibrary pid lid $ zoom Library.body $ do
  --     let graph = lib ^. L.graph
  --         nodes = graph ^. G.nodes
  --         connections = map (\x -> x & _2 %~ InPortRef') $ graph ^. G.connections
  --     {-runASTOp $ mapM_ Graph.addPersistentNode nodes-}
  --     {-runASTOp $ mapM (uncurry Graph.connectPersistent) connections-}
  --     return ()
  -- project <- withProject pid (get >>= return)
  -- return (pid, project)


loadProject :: FilePath -> Empire ProjectId
loadProject path = do
    logger Logger.info $ "Loading project " <> path
    bytes <- liftIO $ BS.readFile path
    let proj = readProject bytes
        basename = takeBaseName path
        maybeProjId = UUID.fromString basename
    case proj of
      Nothing   -> throwError $ "Cannot read JSON from " <> path
      Just proj' -> do
        (pid, _) <- createProjectFromPersistent maybeProjId proj'
        return pid



importProject :: Text -> Empire (ProjectId, Project)
importProject bytes = $notImplemented

exportProject :: ProjectId -> Empire Text
exportProject pid = $notImplemented

defaultProjectName, defaultLibraryName, defaultLibraryPath :: String
defaultProjectName = "default"
defaultLibraryName = "Main"
defaultLibraryPath = "Main.luna"

createDefaultProject :: Empire ()
createDefaultProject = do
  logger Logger.info "Creating default project"
  void $ createLibrary (Just defaultLibraryName) (fromString defaultLibraryPath)
