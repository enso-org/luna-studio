{-# LANGUAGE ScopedTypeVariables #-}
module Empire.Handlers where

import           Prelude               (undefined)
import           Prologue

import qualified Empire.Server.Server                    as Server
import qualified LunaStudio.API.Atom.GetBuffer           as GetBuffer
import qualified LunaStudio.API.Atom.Substitute          as Substitute
import qualified LunaStudio.API.Control.Interpreter      as Interpreter
import qualified LunaStudio.API.Graph.AddConnection      as AddConnection
import qualified LunaStudio.API.Graph.AddImports         as AddImports
import qualified LunaStudio.API.Graph.AddNode            as AddNode
import qualified LunaStudio.API.Graph.AddPort            as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph        as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes    as AutolayoutNodes
import qualified LunaStudio.API.Graph.CollapseToFunction as CollapseToFunction
import qualified LunaStudio.API.Graph.Copy               as Copy
import qualified LunaStudio.API.Graph.DumpGraphViz       as DumpGraphViz
import qualified LunaStudio.API.Graph.GetProgram         as GetProgram
import qualified LunaStudio.API.Graph.GetSubgraphs       as GetSubgraphs
import qualified LunaStudio.API.Graph.MovePort           as MovePort
import qualified LunaStudio.API.Graph.Paste              as Paste
import qualified LunaStudio.API.Graph.RemoveConnection   as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes        as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort         as RemovePort
import qualified LunaStudio.API.Graph.RenameNode         as RenameNode
import qualified LunaStudio.API.Graph.RenamePort         as RenamePort
import qualified LunaStudio.API.Graph.Request            as G
import qualified LunaStudio.API.Graph.SaveSettings       as SaveSettings
import qualified LunaStudio.API.Graph.SearchNodes        as SearchNodes
import qualified LunaStudio.API.Graph.SetCode            as SetCode
import qualified LunaStudio.API.Graph.SetNodeExpression  as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta       as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault     as SetPortDefault
import qualified LunaStudio.API.Graph.Transaction        as Transaction
import qualified LunaStudio.API.Graph.TypeCheck          as TypeCheck

import           Control.Monad.State   (StateT)
import qualified Data.Binary           as Bin
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Lazy  as BSL
import           Data.Constraint
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Empire.ApiHandlers    (Modification)
import           Empire.Env            (Env)
import qualified Empire.Server.Atom    as Atom
import qualified Empire.Server.Graph   as Graph
import qualified Empire.Server.Library as Library
import qualified LunaStudio.API.Topic  as Topic
import           ZMQ.Bus.Trans         (BusT (..))

type Handler = BSL.ByteString -> StateT Env BusT ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ makeHandler $ Server.handle @AddConnection.Request
    , makeHandler $ Server.handle @AddImports.Request
    , makeHandler $ Server.handle @AddNode.Request
    , makeHandler $ Server.handle @AddPort.Request
    , makeHandler $ Server.handle @AddSubgraph.Request
    , makeHandler $ Server.handle @AutolayoutNodes.Request
    , makeHandler $ Server.handle @CollapseToFunction.Request
    , makeHandler $ Server.handle @Copy.Request
    , makeHandler $ Server.handleOk @DumpGraphViz.Request
    , makeHandler Graph.handleGetProgram
    , makeHandler $ Server.handle @GetSubgraphs.Request
    , makeHandler $ Server.handle @MovePort.Request
    , makeHandler $ Server.handle @Paste.Request
    , makeHandler $ Server.handle @RemoveConnection.Request
    , makeHandler $ Server.handle @RemoveNodes.Request
    , makeHandler $ Server.handle @RemovePort.Request
    , makeHandler $ Server.handle @RenameNode.Request
    , makeHandler $ Server.handle @RenamePort.Request
    , makeHandler Graph.handleSearchNodes
    , makeHandler $ Server.handle @SetCode.Request
    , makeHandler $ Server.handle @SetNodeExpression.Request
    , makeHandler $ Server.handle @SetNodesMeta.Request
    , makeHandler $ Server.handle @SetPortDefault.Request
    , makeHandler $ Server.handle @Transaction.Request
    , makeHandler Graph.handleTypecheck
    , makeHandler $ Server.handle @Interpreter.Request
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
    , makeHandler $ Server.handle @GetBuffer.Request
    , makeHandler $ Server.handle @Substitute.Request
    , makeHandler $ Server.handleOk @SaveSettings.Request
    ]


makeHandler :: forall a. (Topic.MessageTopic a, Bin.Binary a) => (a -> StateT Env BusT ()) -> (String, Handler)
makeHandler h = (Topic.topic @a, process) where
   process content = h request where request = Bin.decode content
