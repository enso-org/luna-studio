{-# LANGUAGE ScopedTypeVariables #-}
module Empire.Handlers where

import           Prelude               (undefined)
import           Prologue

import           Control.Monad.State   (StateT)
import qualified Data.Binary           as Bin
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Lazy  as BSL
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Empire.Env            (Env)
import qualified Empire.Server.Atom    as Atom
import qualified Empire.Server.Graph   as Graph
import qualified Empire.Server.Library as Library
import qualified LunaStudio.API.Topic  as Topic
import           ZMQ.Bus.Trans         (BusT (..))

type Handler = BSL.ByteString -> StateT Env BusT ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ makeHandler Graph.handleAddConnection
    , makeHandler Graph.handleAddImports
    , makeHandler Graph.handleAddNode
    , makeHandler Graph.handleAddPort
    , makeHandler Graph.handleAddSubgraph
    , makeHandler Graph.handleAutolayoutNodes
    , makeHandler Graph.handleCollapseToFunction
    , makeHandler Graph.handleCopy
    , makeHandler Graph.handleDumpGraphViz
    , makeHandler Graph.handleGetProgram
    , makeHandler Graph.handleGetSubgraphs
    , makeHandler Graph.handleMovePort
    , makeHandler Graph.handlePaste
    , makeHandler Graph.handleRemoveConnection
    , makeHandler Graph.handleRemoveNodes
    , makeHandler Graph.handleRemovePort
    , makeHandler Graph.handleRenameNode
    , makeHandler Graph.handleRenamePort
    , makeHandler Graph.handleSearchNodes
    , makeHandler Graph.handleSetCode
    , makeHandler Graph.handleSetNodeExpression
    , makeHandler Graph.handleSetNodesMeta
    , makeHandler Graph.handleSetPortDefault
    , makeHandler Graph.handleTypecheck
    , makeHandler Graph.handleInterpreterControl
    , makeHandler Library.handleCreateLibrary
    , makeHandler Library.handleListLibraries
    , makeHandler Atom.handleIsSaved
    , makeHandler Atom.handleMoveProject
    , makeHandler Atom.handleCreateProject
    , makeHandler Atom.handleSetProject
    , makeHandler Atom.handleOpenFile
    , makeHandler Atom.handleSaveFile
    , makeHandler Atom.handleCloseFile
    , makeHandler Atom.handleCopyText
    , makeHandler Atom.handlePasteText
    , makeHandler Graph.handleGetBuffer
    , makeHandler Graph.handleSubstitute
    , makeHandler Graph.handleSaveSettings
    ]

makeHandler :: forall a. (Topic.MessageTopic a, Bin.Binary a) => (a -> StateT Env BusT ()) -> (String, Handler)
makeHandler h = (Topic.topic (undefined :: a), process) where
   process content = h request where request = Bin.decode content
