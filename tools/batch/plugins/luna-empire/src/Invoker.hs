{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Data.Binary                           as Bin
import           Data.ByteString.Lazy                  (toStrict)
import qualified Data.Text                             as Text
import qualified Data.UUID.V4                          as UUID
import           GHC.IO.Encoding                       (setLocaleEncoding, utf8)
import qualified LunaStudio.API.Graph.AddConnection    as AddConnection
import qualified LunaStudio.API.Graph.AddNode          as AddNode
import qualified LunaStudio.API.Graph.DumpGraphViz     as DumpGraphViz
import qualified LunaStudio.API.Graph.GetProgram       as GetProgram
import qualified LunaStudio.API.Graph.RemoveConnection as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes      as RemoveNodes
import qualified LunaStudio.API.Graph.SetNodesMeta     as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault   as SetPortDefault
import qualified LunaStudio.API.Graph.TypeCheck        as TypeCheck
import qualified LunaStudio.API.Library.CreateLibrary  as CreateLibrary
import qualified LunaStudio.API.Library.ListLibraries  as ListLibraries
import           LunaStudio.API.Request                (Request (..))
import qualified LunaStudio.API.Topic                  as Topic
import qualified LunaStudio.Data.Breadcrumb            as Breadcrumb
import           LunaStudio.Data.GraphLocation         (GraphLocation)
import qualified LunaStudio.Data.GraphLocation         as GraphLocation
import           LunaStudio.Data.NodeId                (NodeId)
import           LunaStudio.Data.NodeLoc               (NodeLoc (..))
import qualified LunaStudio.Data.NodeMeta              as NodeMeta
import           LunaStudio.Data.Port                  (InPortId, InPortIndex (..), OutPortId)
import           LunaStudio.Data.PortDefault           (PortDefault (Constant), PortValue (RealValue))
import           LunaStudio.Data.PortRef               (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.Position              as Position
import           LunaStudio.Data.Project               (ProjectId)
import           Prologue                              hiding (argument)
import           System.Console.Docopt
import           System.Environment                    (getArgs)
import qualified ZMQ.Bus.Bus                           as Bus
import qualified ZMQ.Bus.Config                        as Config
import qualified ZMQ.Bus.Data.Flag                     as Flag
import qualified ZMQ.Bus.Data.Message                  as Message
import qualified ZMQ.Bus.EndPoint                      as EP


toGraphLocation :: FilePath -> GraphLocation
toGraphLocation file = GraphLocation.GraphLocation file (Breadcrumb.Breadcrumb [])

patterns :: Docopt
patterns = [docoptFile|src/InvokerUsage.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    setLocaleEncoding utf8
    args <- parseArgsOrExit patterns =<< getArgs
    endPoints <- EP.clientFromConfig <$> Config.load
    when (args `isPresent` command "addNode") $ do
        file      <- args `getArgOrExit` argument "file"
        nodeId    <- args `getArgOrExit` argument "nodeId"
        expr      <- args `getArgOrExit` argument "expression"
        x         <- args `getArgOrExit` argument "x"
        y         <- args `getArgOrExit` argument "y"
        addNode endPoints (toGraphLocation file) (unsafeRead nodeId) expr (unsafeRead x) (unsafeRead y)
    when (args `isPresent` command "removeNode") $ do
        file      <- args `getArgOrExit` argument "file"
        nodeId    <- args `getArgOrExit` argument "nodeId"
        removeNode endPoints (toGraphLocation file) (unsafeRead nodeId)
    when (args `isPresent` command "setNodeMeta") $ do
        file      <- args `getArgOrExit` argument "file"
        nodeId    <- args `getArgOrExit` argument "nodeId"
        x         <- args `getArgOrExit` argument "x"
        y         <- args `getArgOrExit` argument "y"
        req       <- args `getArgOrExit` argument "req"
        setNodeMeta endPoints (toGraphLocation file) (unsafeRead nodeId) (unsafeRead x) (unsafeRead y) (unsafeRead req)
    when (args `isPresent` command "connect") $ do
        file      <- args `getArgOrExit` argument "file"
        srcNodeId <- args `getArgOrExit` argument "srcNodeId"
        outPort   <- args `getArgOrExit` argument "outPort"
        dstNodeId <- args `getArgOrExit` argument "dstNodeId"
        inPort    <- args `getArgOrExit` argument "inPort"
        connect endPoints (toGraphLocation file) (unsafeRead srcNodeId) (unsafeRead outPort) (unsafeRead dstNodeId) (unsafeRead inPort)
    when (args `isPresent` command "disconnect") $ do
        file      <- args `getArgOrExit` argument "file"
        dstNodeId <- args `getArgOrExit` argument "dstNodeId"
        inPort    <- args `getArgOrExit` argument "inPort"
        disconnect endPoints (toGraphLocation file) (unsafeRead dstNodeId) (unsafeRead inPort)
    when (args `isPresent` command "setValue") $ do
        file      <- args `getArgOrExit` argument "file"
        nodeId    <- args `getArgOrExit` argument "nodeId"
        portId    <- args `getArgOrExit` argument "portId"
        value     <- args `getArgOrExit` argument "value"
        setPortValue endPoints (toGraphLocation file) (unsafeRead nodeId) (unsafeRead portId) (unsafeRead value)
    when (args `isPresent` command "getProgram") $ do
        file      <- args `getArgOrExit` argument "file"
        getProgram endPoints (toGraphLocation file)
    when (args `isPresent` command "createLibrary") $ do
        pid       <- args `getArgOrExit` argument "pid"
        path      <- args `getArgOrExit` argument "path"
        let name   = args `getArg`       argument "name"
        createLibrary endPoints (unsafeRead pid) name path
    when (args `isPresent` command "libraries") $ do
        pid       <- args `getArgOrExit` argument "pid"
        listLibraries endPoints $ unsafeRead pid
    when (args `isPresent` command "graphviz") $ do
        file      <- args `getArgOrExit` argument "file"
        environmentDumpGraphviz endPoints $ toGraphLocation file
    when (args `isPresent` command "typecheck") $ do
        file      <- args `getArgOrExit` argument "file"
        typecheck endPoints $ toGraphLocation file

sendToBus :: (Topic.MessageTopic (Request a), Bin.Binary a) => EP.BusEndPoints -> a -> IO ()
sendToBus endPoints msg = do
  uuid <- UUID.nextRandom
  let msg' = Request uuid Nothing msg
  void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message (Topic.topic msg') $ toStrict . Bin.encode $ msg'

addNode :: EP.BusEndPoints -> GraphLocation -> NodeId -> String -> Double -> Double -> IO ()
addNode endPoints graphLocation nodeId expression x y = sendToBus endPoints $ AddNode.Request graphLocation (NodeLoc def nodeId) (Text.pack expression) (NodeMeta.NodeMeta (Position.fromTuple (x, y)) True def) Nothing

removeNode :: EP.BusEndPoints -> GraphLocation -> NodeId -> IO ()
removeNode endPoints graphLocation nodeId = sendToBus endPoints $ RemoveNodes.Request graphLocation [convert nodeId]

setNodeMeta :: EP.BusEndPoints -> GraphLocation -> NodeId -> Double -> Double -> Bool -> IO ()
setNodeMeta endPoints graphLocation nodeId x y req = sendToBus endPoints $ SetNodesMeta.Request graphLocation [(nodeId, NodeMeta.NodeMeta (Position.fromTuple (x, y)) req def)]

connect :: EP.BusEndPoints -> GraphLocation -> NodeId -> OutPortId -> NodeId -> InPortId -> IO ()
connect endPoints graphLocation srcNodeId outPort dstNodeId inPort = sendToBus endPoints $ AddConnection.Request graphLocation (Left $ OutPortRef (NodeLoc def srcNodeId) outPort) (Left . InPortRef' $ InPortRef (NodeLoc def dstNodeId) inPort)

disconnect :: EP.BusEndPoints -> GraphLocation -> NodeId -> InPortId -> IO ()
disconnect endPoints graphLocation  dstNodeId inPort = sendToBus endPoints $ RemoveConnection.Request graphLocation (InPortRef (NodeLoc def dstNodeId) inPort)

setPortValue :: EP.BusEndPoints -> GraphLocation -> NodeId -> Int -> Double -> IO ()
setPortValue endPoints graphLocation nodeId portId value = sendToBus endPoints $ SetPortDefault.Request graphLocation (InPortRef (NodeLoc def nodeId) [Arg portId]) (Just $ Constant $ RealValue value)

getProgram :: EP.BusEndPoints -> GraphLocation -> IO ()
getProgram endPoints graphLocation = sendToBus endPoints $ GetProgram.Request graphLocation def False

createLibrary :: EP.BusEndPoints -> ProjectId -> Maybe String -> String -> IO ()
createLibrary endPoints pid name path = sendToBus endPoints $ CreateLibrary.Request pid name path

listLibraries :: EP.BusEndPoints -> ProjectId -> IO ()
listLibraries endPoints pid = sendToBus endPoints $ ListLibraries.Request pid

environmentDumpGraphviz :: EP.BusEndPoints -> GraphLocation -> IO ()
environmentDumpGraphviz endPoints loc = sendToBus endPoints $ DumpGraphViz.Request loc

typecheck :: EP.BusEndPoints -> GraphLocation -> IO ()
typecheck endPoints loc = sendToBus endPoints $ TypeCheck.Request loc
