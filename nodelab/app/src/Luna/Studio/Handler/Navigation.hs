module Luna.Studio.Handler.Navigation where

import qualified Data.HashMap.Strict          as HashMap

import           Data.Position                (Position (Position), lengthSquared, magnitude, vector, x, y)
import           Luna.Studio.Prelude

import qualified Empire.API.Data.Connection   as C
import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Port         as P
import qualified Empire.API.Data.PortRef      as R
import           Luna.Studio.Action.Command   (Command)
import qualified Luna.Studio.Action.Graph     as Graph
import           Luna.Studio.Event.Event      (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut   as Shortcut
import           Luna.Studio.React.Model.Node (Node)
import qualified Luna.Studio.React.Model.Node as Node
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.Graph      as Graph



handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _)) = Just $ handleCommand command
handle _ = Nothing

handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.GoConeDown  -> goConeDown
    Shortcut.GoConeLeft  -> goConeLeft
    Shortcut.GoConeRight -> goConeRight
    Shortcut.GoConeUp    -> goConeUp
    Shortcut.GoDown      -> goDown
    Shortcut.GoLeft      -> goLeft
    Shortcut.GoNext      -> goNext
    Shortcut.GoPrev      -> goPrev
    Shortcut.GoRight     -> goRight
    Shortcut.GoUp        -> goUp
    _                    -> return ()

goPrev :: Command State ()
goPrev = do
    selectedNodes <- Graph.selectedNodes
    unless (null selectedNodes) $ do
        let nodeSrc = findLeftMost selectedNodes
            nodeId = nodeSrc ^. Node.nodeId
            inPortRefSelf      = R.InPortRef nodeId P.Self
            inPortRefFirstPort = R.InPortRef nodeId $ P.Arg 0
        prevSelfNodeIdMay <- preuse $ Global.graph . Graph.connectionsMap . ix inPortRefSelf . C.src . R.srcNodeId
        case prevSelfNodeIdMay of
            Just prevSelfNodeId -> Graph.selectNodes [prevSelfNodeId]
            Nothing -> do
                prevFirstPortNodeIdMay <- preuse $ Global.graph . Graph.connectionsMap . ix inPortRefFirstPort . C.src . R.srcNodeId
                withJust prevFirstPortNodeIdMay $ Graph.selectNodes . return

goNext :: Command State ()
goNext = do
    selectedNodes <- Graph.selectedNodes
    unless (null selectedNodes) $ do
        let nodeSrc = findRightMost selectedNodes
            nodeId = nodeSrc ^. Node.nodeId
        nextNodeIds <- getDstNodeIds nodeId
        nextNodes <- catMaybes <$> mapM Global.getNode nextNodeIds
        unless (null nextNodes) $ do
            let nextNode = findUpMost nextNodes
            Graph.selectNodes [nextNode ^. Node.nodeId]

getDstNodeIds :: NodeId -> Command State [NodeId]
getDstNodeIds nodeId = do
    connMap <- use $ Global.graph . Graph.connectionsMap
    let connections = filter matchNodeId $ HashMap.elems connMap
    return $ (^. C.dst . R.dstNodeId) <$> connections
    where
        matchNodeId conn = conn ^. C.src . R.srcNodeId == nodeId

goRight, goLeft, goDown, goUp :: Command State ()
goRight = go findRightMost findNodesOnRightSide findNearestRight
goLeft  = go findLeftMost  findNodesOnLeftSide  findNearestLeft
goDown  = go findDownMost  findNodesOnDownSide  findNearestDown
goUp    = go findUpMost    findNodesOnUpSide    findNearestUp

go :: ([Node] -> Node) ->
      (Position -> [Node] -> [Node]) ->
      (Position -> [Node] -> Node) ->
      Command State ()
go findMost findNodesOnSide findNearest = do
    nodes         <- Graph.allNodes
    selectedNodes <- Graph.selectedNodes
    unless (null selectedNodes) $ do
        let nodeSrc = findMost selectedNodes
            pos = nodeSrc ^. Node.position
            nodesSide = findNodesOnSide pos nodes
        unless (null nodesSide) $ do
            Graph.selectNodes [findNearest pos nodesSide ^. Node.nodeId]

closenestPow :: Double
closenestPow = 2.5

axisDistanceRight, axisDistanceLeft, axisDistanceDown, axisDistanceUp :: Position -> Double
axisDistanceRight pos =   pos ^. x
axisDistanceLeft  pos = -(pos ^. x)
axisDistanceDown  pos =   pos ^. y
axisDistanceUp    pos = -(pos ^. y)

findNearestRight, findNearestLeft, findNearestDown, findNearestUp :: Position -> [Node] -> Node
findNearestRight pos = maximumBy (compare `on` closenest pos axisDistanceRight)
findNearestLeft  pos = maximumBy (compare `on` closenest pos axisDistanceLeft)
findNearestDown  pos = maximumBy (compare `on` closenest pos axisDistanceDown)
findNearestUp    pos = maximumBy (compare `on` closenest pos axisDistanceUp)

closenest :: Position -> (Position -> Double) -> Node -> Double
closenest pos axisDistance node = axisDist / (dist ** closenestPow) where
    pos' = node ^. Node.position
    vect = pos' ^. vector - pos ^. vector
    dist = magnitude vect
    axisDist = axisDistance (Position vect)

goConeRight, goConeLeft, goConeDown, goConeUp :: Command State ()
goConeRight = goCone findRightMost findNodesOnRight findNodesOnRightSide
goConeLeft  = goCone findLeftMost  findNodesOnLeft  findNodesOnLeftSide
goConeDown  = goCone findDownMost  findNodesOnDown  findNodesOnDownSide
goConeUp    = goCone findUpMost    findNodesOnUp    findNodesOnUpSide

goCone :: ([Node] -> Node) ->
          (Position -> [Node] -> [Node]) ->
          (Position -> [Node] -> [Node]) ->
          Command State ()
goCone findMost findNodesInCone findNodesOnSide = do
    nodes         <- Graph.allNodes
    selectedNodes <- Graph.selectedNodes
    unless (null selectedNodes) $ do
        let nodeSrc = findMost selectedNodes
            pos = nodeSrc ^. Node.position
            nodesCone = findNodesInCone pos nodes
            nodesSide = findNodesOnSide pos nodes
        if not $ null nodesCone
            then                           Graph.selectNodes [findNearestNode pos nodesCone ^. Node.nodeId]
            else unless (null nodesSide) $ Graph.selectNodes [findNearestNode pos nodesSide ^. Node.nodeId]

findRightMost, findLeftMost, findDownMost, findUpMost :: [Node] -> Node
findRightMost = maximumBy (compare `on` (^. Node.position . x))
findLeftMost  = minimumBy (compare `on` (^. Node.position . x))
findDownMost  = maximumBy (compare `on` (^. Node.position . y))
findUpMost    = minimumBy (compare `on` (^. Node.position . y))

findNodesOnRightSide, findNodesOnLeftSide, findNodesOnDownSide, findNodesOnUpSide :: Position -> [Node] -> [Node]
findNodesOnRightSide pos = filter $ \node -> node ^. Node.position . x > pos ^. x
findNodesOnLeftSide  pos = filter $ \node -> node ^. Node.position . x < pos ^. x
findNodesOnDownSide  pos = filter $ \node -> node ^. Node.position . y > pos ^. y
findNodesOnUpSide    pos = filter $ \node -> node ^. Node.position . y < pos ^. y

findNodesOnRight, findNodesOnLeft, findNodesOnDown, findNodesOnUp :: Position -> [Node] -> [Node]
findNodesOnRight = filter . isOnRight
findNodesOnLeft  = filter . isOnLeft
findNodesOnDown  = filter . isOnDown
findNodesOnUp    = filter . isOnUp

isOnRight, isOnLeft, isOnDown, isOnUp :: Position -> Node -> Bool
isOnRight = isInCone (>)  skip (>=)
isOnLeft  = isInCone (<)  skip (>=)
isOnDown  = isInCone skip (>)  (<)
isOnUp    = isInCone skip (<)  (<)

skip :: Double -> Double -> Bool
skip _ _ = True

isInCone :: (Double -> Double -> Bool) -> (Double -> Double -> Bool) -> (Double -> Double -> Bool) -> Position -> Node -> Bool
isInCone cmpDXZero cmpDYZero cmpDims pos node = dx `cmpDXZero` 0.0 && dy `cmpDYZero` 0.0 && abs dx `cmpDims` abs dy where
    nodePos = node ^. Node.position
    dx = nodePos ^. x - pos ^. x
    dy = nodePos ^. y - pos ^. y

findNearestNode :: Position -> [Node] -> Node
findNearestNode pos = minimumBy (compare `on` distance pos)

distance :: Position -> Node -> Double
distance pos node = lengthSquared (wpos ^. vector - pos ^. vector) where
    wpos = node ^. Node.position
