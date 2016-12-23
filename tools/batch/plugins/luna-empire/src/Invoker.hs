{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Data.Binary                      as Bin
import qualified Data.ByteString                  as ByteString
import qualified Data.ByteString.Char8            as Char8 (pack)
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import qualified Data.UUID.V4                     as UUID
import qualified Empire.API.Data.Breadcrumb       as Breadcrumb
import           Empire.API.Data.DefaultValue     (PortDefault (Constant), Value (DoubleValue))
import           Empire.API.Data.GraphLocation    (GraphLocation)
import qualified Empire.API.Data.GraphLocation    as GraphLocation
import           Empire.API.Data.Node             (NodeId)
import qualified Empire.API.Data.Node             as Node
import qualified Empire.API.Data.NodeMeta         as NodeMeta
import qualified Empire.API.Data.NodeMeta         as NodeMeta
import           Empire.API.Data.Port             (InPort (..), OutPort)
import           Empire.API.Data.PortRef          (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           Empire.API.Data.Project          (ProjectId)
import qualified Empire.API.Graph.AddNode         as AddNode
import qualified Empire.API.Graph.Connect         as Connect
import qualified Empire.API.Graph.Disconnect      as Disconnect
import qualified Empire.API.Graph.DumpGraphViz    as DumpGraphViz
import qualified Empire.API.Graph.GetProgram      as GetProgram
import qualified Empire.API.Graph.RemoveNodes     as RemoveNodes
import qualified Empire.API.Graph.SetDefaultValue as SetDefaultValue
import qualified Empire.API.Graph.TypeCheck       as TypeCheck
import qualified Empire.API.Graph.UpdateNodeMeta  as UpdateNodeMeta
import qualified Empire.API.Library.CreateLibrary as CreateLibrary
import qualified Empire.API.Library.ListLibraries as ListLibraries
import qualified Empire.API.Project.CreateProject as CreateProject
import qualified Empire.API.Project.ListProjects  as ListProjects
import           Empire.API.Request               (Request (..))
import qualified Empire.API.Response              as Response
import qualified Empire.API.Topic                 as Topic
import           Prologue                         hiding (argument)
import           System.Console.Docopt
import           System.Environment               (getArgs)
import           System.Log.Options               (help, long, metavar, short)
import qualified System.Log.Options               as Opt
import qualified ZMQ.Bus.Bus                      as Bus
import qualified ZMQ.Bus.Config                   as Config
import qualified ZMQ.Bus.Data.Flag                as Flag
import qualified ZMQ.Bus.Data.Message             as Message
import qualified ZMQ.Bus.EndPoint                 as EP


toGraphLocation :: String -> String -> GraphLocation
toGraphLocation pid lid = GraphLocation.GraphLocation (read pid) (read lid) (Breadcrumb.Breadcrumb [])

patterns :: Docopt
patterns = [docoptFile|src/InvokerUsage.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    endPoints <- EP.clientFromConfig <$> Config.load
    when (args `isPresent` command "addNode") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        expr      <- args `getArgOrExit` argument "expression"
        x         <- args `getArgOrExit` argument "x"
        y         <- args `getArgOrExit` argument "y"
        addNode endPoints (toGraphLocation pid lid) expr (read x) (read y)
    when (args `isPresent` command "removeNode") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        nodeId    <- args `getArgOrExit` argument "nodeId"
        removeNode endPoints (toGraphLocation pid lid) (read nodeId)
    when (args `isPresent` command "updateNodeMeta") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        nodeId    <- args `getArgOrExit` argument "nodeId"
        x         <- args `getArgOrExit` argument "x"
        y         <- args `getArgOrExit` argument "y"
        req       <- args `getArgOrExit` argument "req"
        updateNodeMeta endPoints (toGraphLocation pid lid) (read nodeId) (read x) (read y) (read req)
    when (args `isPresent` command "connect") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        srcNodeId <- args `getArgOrExit` argument "srcNodeId"
        outPort   <- args `getArgOrExit` argument "outPort"
        dstNodeId <- args `getArgOrExit` argument "dstNodeId"
        inPort    <- args `getArgOrExit` argument "inPort"
        connect endPoints (toGraphLocation pid lid) (read srcNodeId) (read outPort) (read dstNodeId) (read inPort)
    when (args `isPresent` command "disconnect") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        dstNodeId <- args `getArgOrExit` argument "dstNodeId"
        inPort    <- args `getArgOrExit` argument "inPort"
        disconnect endPoints (toGraphLocation pid lid) (read dstNodeId) (read inPort)
    when (args `isPresent` command "setValue") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        nodeId    <- args `getArgOrExit` argument "nodeId"
        portId    <- args `getArgOrExit` argument "portId"
        value     <- args `getArgOrExit` argument "value"
        setPortValue endPoints (toGraphLocation pid lid) (read nodeId) (read portId) (read value)
    when (args `isPresent` command "getProgram") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        getProgram endPoints (toGraphLocation pid lid)
    when (args `isPresent` command "createProject") $ do
        name      <- args `getArgOrExit` argument "name"
        createProject endPoints name
    when (args `isPresent` command "createLibrary") $ do
        pid       <- args `getArgOrExit` argument "pid"
        path      <- args `getArgOrExit` argument "path"
        let name   = args `getArg`       argument "name"
        createLibrary endPoints (read pid) name path
    when (args `isPresent` command "projects") $
        listProjects endPoints
    when (args `isPresent` command "libraries") $ do
        pid       <- args `getArgOrExit` argument "pid"
        listLibraries endPoints $ read pid
    when (args `isPresent` command "graphviz") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        environmentDumpGraphviz endPoints $ toGraphLocation pid lid
    when (args `isPresent` command "typecheck") $ do
        pid       <- args `getArgOrExit` argument "pid"
        lid       <- args `getArgOrExit` argument "lid"
        typecheck endPoints $ toGraphLocation pid lid

sendToBus :: (Topic.MessageTopic (Request a), Bin.Binary a) => EP.BusEndPoints -> a -> IO ()
sendToBus endPoints msg = do
  uuid <- UUID.nextRandom
  let msg' = Request uuid msg
  void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message (Topic.topic msg') $ toStrict . Bin.encode $ msg'

addNode :: EP.BusEndPoints -> GraphLocation -> String -> Double -> Double -> IO ()
addNode endPoints graphLocation expression x y = sendToBus endPoints $ AddNode.Request graphLocation (AddNode.ExpressionNode $ convert expression) (NodeMeta.NodeMeta (x, y) True) Nothing

removeNode :: EP.BusEndPoints -> GraphLocation -> NodeId -> IO ()
removeNode endPoints graphLocation nodeId = sendToBus endPoints $ RemoveNodes.Request graphLocation [nodeId]

updateNodeMeta :: EP.BusEndPoints -> GraphLocation -> NodeId -> Double -> Double -> Bool -> IO ()
updateNodeMeta endPoints graphLocation nodeId x y req = sendToBus endPoints $ UpdateNodeMeta.Request graphLocation [(nodeId, NodeMeta.NodeMeta (x, y) req)]

connect :: EP.BusEndPoints -> GraphLocation -> NodeId -> OutPort -> NodeId -> InPort -> IO ()
connect endPoints graphLocation srcNodeId outPort dstNodeId inPort = sendToBus endPoints $ Connect.Request graphLocation (OutPortRef srcNodeId outPort) (InPortRef dstNodeId inPort)

disconnect :: EP.BusEndPoints -> GraphLocation -> NodeId -> InPort -> IO ()
disconnect endPoints graphLocation  dstNodeId inPort = sendToBus endPoints $ Disconnect.Request graphLocation (InPortRef dstNodeId inPort)

setPortValue :: EP.BusEndPoints -> GraphLocation -> NodeId -> Int -> Double -> IO ()
setPortValue endPoints graphLocation nodeId portId value = sendToBus endPoints $ SetDefaultValue.Request graphLocation (InPortRef' $ InPortRef nodeId (Arg portId)) (Constant $ DoubleValue value)

getProgram :: EP.BusEndPoints -> GraphLocation -> IO ()
getProgram endPoints graphLocation = sendToBus endPoints $ GetProgram.Request graphLocation

createProject :: EP.BusEndPoints -> String -> IO ()
createProject endPoints name = sendToBus endPoints $ CreateProject.Request name

listProjects :: EP.BusEndPoints -> IO ()
listProjects endPoints = sendToBus endPoints ListProjects.Request

createLibrary :: EP.BusEndPoints -> ProjectId -> Maybe String -> String -> IO ()
createLibrary endPoints pid name path = sendToBus endPoints $ CreateLibrary.Request pid name path

listLibraries :: EP.BusEndPoints -> ProjectId -> IO ()
listLibraries endPoints pid = sendToBus endPoints $ ListLibraries.Request pid

environmentDumpGraphviz :: EP.BusEndPoints -> GraphLocation -> IO ()
environmentDumpGraphviz endPoints loc = sendToBus endPoints $ DumpGraphViz.Request loc

typecheck :: EP.BusEndPoints -> GraphLocation -> IO ()
typecheck endPoints loc = sendToBus endPoints $ TypeCheck.Request loc
