{-# LANGUAGE ScopedTypeVariables #-}
module Empire.Handlers where

import           Prelude               (undefined)
import           Prologue

import           Control.Monad.State   (StateT)
import qualified Data.Binary           as Bin
import           Data.ByteString       (ByteString)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Lazy  (fromStrict)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Empire.API.Topic      as Topic
import           Empire.Env            (Env)
import qualified Empire.Server.Graph   as Graph
import qualified Empire.Server.Library as Library
import qualified Empire.Server.Project as Project
import           ZMQ.Bus.Trans         (BusT (..))


type Handler = ByteString -> StateT Env BusT ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ makeHandler Graph.handleAddNode
    , makeHandler Graph.handleAddSubgraph
    , makeHandler Graph.handleRemoveNodes
    , makeHandler Graph.handleUpdateNodeExpression
    , makeHandler Graph.handleUpdateNodeMeta
    , makeHandler Graph.handleRenameNode
    , makeHandler Graph.handleConnect
    , makeHandler Graph.handleDisconnect
    , makeHandler Graph.handleSetDefaultValue
    , makeHandler Graph.handleGetProgram
    , makeHandler Graph.handleDumpGraphViz
    , makeHandler Graph.handleTypecheck
    , makeHandler Project.handleCreateProject
    , makeHandler Project.handleExportProject
    , makeHandler Project.handleImportProject
    , makeHandler Project.handleListProjects
    , makeHandler Project.handleOpenProject
    , makeHandler Library.handleCreateLibrary
    , makeHandler Library.handleListLibraries
    ]

makeHandler :: forall a. (Topic.MessageTopic a, Bin.Binary a) => (a -> StateT Env BusT ()) -> (String, Handler)
makeHandler h = (Topic.topic (undefined :: a), process) where
   process content = h request where request = Bin.decode . fromStrict $ content
