{-# LANGUAGE ScopedTypeVariables #-}
module Empire.Handlers where

import           Prelude               (undefined)
import           Prologue

import qualified Codec.Compression.GZip as GZip
import           Control.Monad.State   (StateT)
import qualified Data.Binary           as Bin
import           Data.ByteString       (ByteString)
import           Data.ByteString.Lazy  (fromStrict)
import qualified Data.ByteString.Lazy  as BSL
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified LunaStudio.API.Topic      as Topic
import           Empire.Env            (Env)
import qualified Empire.Server.Atom    as Atom
import qualified Empire.Server.Graph   as Graph
import qualified Empire.Server.Library as Library
import qualified Empire.Server.Project as Project
import           ZMQ.Bus.Trans         (BusT (..))

import System.IO.Unsafe (unsafePerformIO)

type Handler = ByteString -> StateT Env BusT ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ makeHandler Graph.handleAddConnection
    , makeHandler Graph.handleAddNode
    , makeHandler Graph.handleAddPort
    , makeHandler Graph.handleAddSubgraph
    , makeHandler Graph.handleAutolayoutNodes
    , makeHandler Graph.handleDumpGraphViz
    , makeHandler Graph.handleGetProgram
    , makeHandler Graph.handleGetSubgraphs
    , makeHandler Graph.handleMovePort
    , makeHandler Graph.handleRemoveConnection
    , makeHandler Graph.handleRemoveNodes
    , makeHandler Graph.handleRemovePort
    , makeHandler Graph.handleRenameNode
    , makeHandler Graph.handleRenamePort
    , makeHandler Graph.handleSearchNodes
    , makeHandler Graph.handleSetNodeExpression
    , makeHandler Graph.handleSetNodesMeta
    , makeHandler Graph.handleSetPortDefault
    , makeHandler Graph.handleTypecheck
    , makeHandler Library.handleCreateLibrary
    , makeHandler Library.handleListLibraries
    , makeHandler Project.handleCreateProject
    , makeHandler Project.handleExportProject
    , makeHandler Project.handleImportProject
    , makeHandler Project.handleListProjects
    , makeHandler Project.handleOpenProject
    , makeHandler Atom.handleIsSaved
    , makeHandler Atom.handleSetProject
    , makeHandler Atom.handleOpenFile
    , makeHandler Atom.handleSaveFile
    , makeHandler Atom.handleCloseFile
    , makeHandler Graph.handleGetBuffer
    , makeHandler Graph.handleSubstitute
    ]

decompressWithDebug :: BSL.ByteString -> BSL.ByteString
decompressWithDebug d = unsafePerformIO $ do
    putStrLn "===== Decompressing data ====="
    print d
    putStrLn "===== o takie ================"
    return $ GZip.decompress d

makeHandler :: forall a. (Topic.MessageTopic a, Bin.Binary a) => (a -> StateT Env BusT ()) -> (String, Handler)
makeHandler h = (Topic.topic (undefined :: a), process) where
   process content = h request where request = Bin.decode . decompressWithDebug . fromStrict $ content
