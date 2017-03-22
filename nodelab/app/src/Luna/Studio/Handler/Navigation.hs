module Luna.Studio.Handler.Navigation where

import           Data.Position                               (Position (Position), vector, x, y)
import           Data.Vector                                 (lengthSquared, magnitude)
import           Luna.Studio.Prelude

import qualified Empire.API.Data.Port                        as P
import qualified Empire.API.Data.PortRef                     as R
import           Luna.Studio.Action.Basic                    (selectNodes)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getConnection, getConnections, getExpressionNode, getExpressionNodes,
                                                              getSelectedNodes)
import           Luna.Studio.Action.State.Scene              (getScreenCenter, translateToWorkspace)
import           Luna.Studio.Event.Event                     (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut                  as Shortcut
import qualified Luna.Studio.React.Model.Connection          as C
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, NodeId, nodeId, position)
import           Luna.Studio.State.Global                    (State)


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

selectAny :: Command State ()
selectAny = do
    withJustM getScreenCenter $ \screenCenter -> do
        workspaceCenter <- translateToWorkspace screenCenter
        nodes <- getExpressionNodes
        unless (null nodes) $ do
            let node = findNearestNode workspaceCenter nodes
            selectNodes [node ^. nodeId]

goPrev :: Command State ()
goPrev = do
    selectedNodes <- getSelectedNodes
    if null selectedNodes then selectAny
    else do
        let nodeSrc = findLeftMost selectedNodes
            nid     = nodeSrc ^. nodeId
            inPortRefSelf      = R.InPortRef nid   P.Self
            inPortRefFirstPort = R.InPortRef nid $ P.Arg 0
        prevSelfNodeIdMay <- view (C.src . R.srcNodeId) <∘> getConnection inPortRefSelf
        case prevSelfNodeIdMay of
            Just prevSelfNodeId -> selectNodes [prevSelfNodeId]
            Nothing -> do
                prevFirstPortNodeIdMay <- view (C.src . R.srcNodeId) <∘> getConnection inPortRefFirstPort
                withJust prevFirstPortNodeIdMay $ selectNodes . return

goNext :: Command State ()
goNext = do
    selectedNodes <- getSelectedNodes
    if null selectedNodes then selectAny
    else do
        let nodeSrc = findRightMost selectedNodes
            nid     = nodeSrc ^. nodeId
        nextNodeIds <- getDstNodeIds nid
        nextNodes   <- catMaybes <$> mapM getExpressionNode nextNodeIds
        unless (null nextNodes) $ do
            let nextNode = findUpMost nextNodes
            selectNodes [nextNode ^. nodeId]

getDstNodeIds :: NodeId -> Command State [NodeId]
getDstNodeIds nid = do
    connections <- filter matchNodeId <$> getConnections
    return $ (^. C.dst . R.dstNodeId) <$> connections
    where
        matchNodeId conn = conn ^. C.src . R.srcNodeId == nid

goRight, goLeft, goDown, goUp :: Command State ()
goRight = go findRightMost findNodesOnRightSide findNearestRight
goLeft  = go findLeftMost  findNodesOnLeftSide  findNearestLeft
goDown  = go findDownMost  findNodesOnDownSide  findNearestDown
goUp    = go findUpMost    findNodesOnUpSide    findNearestUp

go :: ([ExpressionNode] -> ExpressionNode) ->
      (Position -> [ExpressionNode] -> [ExpressionNode]) ->
      (Position -> [ExpressionNode] -> ExpressionNode) ->
      Command State ()
go findMost findNodesOnSide findNearest = do
    nodes         <- getExpressionNodes
    selectedNodes <- getSelectedNodes
    if null selectedNodes then selectAny
    else do
        let nodeSrc = findMost selectedNodes
            pos = nodeSrc ^. position
            nodesSide = findNodesOnSide pos nodes
        unless (null nodesSide) $ do
            selectNodes [findNearest pos nodesSide ^. nodeId]

closenestPow :: Double
closenestPow = 2.5

axisDistanceRight, axisDistanceLeft, axisDistanceDown, axisDistanceUp :: Position -> Double
axisDistanceRight pos =   pos ^. x
axisDistanceLeft  pos = -(pos ^. x)
axisDistanceDown  pos =   pos ^. y
axisDistanceUp    pos = -(pos ^. y)

findNearestRight, findNearestLeft, findNearestDown, findNearestUp :: Position -> [ExpressionNode] -> ExpressionNode
findNearestRight pos = maximumBy (compare `on` closenest pos axisDistanceRight)
findNearestLeft  pos = maximumBy (compare `on` closenest pos axisDistanceLeft)
findNearestDown  pos = maximumBy (compare `on` closenest pos axisDistanceDown)
findNearestUp    pos = maximumBy (compare `on` closenest pos axisDistanceUp)

closenest :: Position -> (Position -> Double) -> ExpressionNode -> Double
closenest pos axisDistance node = axisDist / (dist ** closenestPow) where
    pos' = node ^. position
    vect = pos' ^. vector - pos ^. vector
    dist = magnitude vect
    axisDist = axisDistance (Position vect)

goConeRight, goConeLeft, goConeDown, goConeUp :: Command State ()
goConeRight = goCone findRightMost findNodesOnRight findNodesOnRightSide
goConeLeft  = goCone findLeftMost  findNodesOnLeft  findNodesOnLeftSide
goConeDown  = goCone findDownMost  findNodesOnDown  findNodesOnDownSide
goConeUp    = goCone findUpMost    findNodesOnUp    findNodesOnUpSide

goCone :: ([ExpressionNode] -> ExpressionNode) ->
          (Position -> [ExpressionNode] -> [ExpressionNode]) ->
          (Position -> [ExpressionNode] -> [ExpressionNode]) ->
          Command State ()
goCone findMost findNodesInCone findNodesOnSide = do
    nodes         <- getExpressionNodes
    selectedNodes <- getSelectedNodes
    if null selectedNodes then selectAny
    else do
        let nodeSrc = findMost selectedNodes
            pos = nodeSrc ^. position
            nodesCone = findNodesInCone pos nodes
            nodesSide = findNodesOnSide pos nodes
        if not $ null nodesCone
            then                           selectNodes [findNearestNode pos nodesCone ^. nodeId]
            else unless (null nodesSide) $ selectNodes [findNearestNode pos nodesSide ^. nodeId]

findRightMost, findLeftMost, findDownMost, findUpMost :: [ExpressionNode] -> ExpressionNode
findRightMost = maximumBy (compare `on` (^. position . x))
findLeftMost  = minimumBy (compare `on` (^. position . x))
findDownMost  = maximumBy (compare `on` (^. position . y))
findUpMost    = minimumBy (compare `on` (^. position . y))

findNodesOnRightSide, findNodesOnLeftSide, findNodesOnDownSide, findNodesOnUpSide :: Position -> [ExpressionNode] -> [ExpressionNode]
findNodesOnRightSide pos = filter $ \node -> node ^. position . x > pos ^. x
findNodesOnLeftSide  pos = filter $ \node -> node ^. position . x < pos ^. x
findNodesOnDownSide  pos = filter $ \node -> node ^. position . y > pos ^. y
findNodesOnUpSide    pos = filter $ \node -> node ^. position . y < pos ^. y

findNodesOnRight, findNodesOnLeft, findNodesOnDown, findNodesOnUp :: Position -> [ExpressionNode] -> [ExpressionNode]
findNodesOnRight = filter . isOnRight
findNodesOnLeft  = filter . isOnLeft
findNodesOnDown  = filter . isOnDown
findNodesOnUp    = filter . isOnUp

isOnRight, isOnLeft, isOnDown, isOnUp :: Position -> ExpressionNode -> Bool
isOnRight = isInCone (>)  skip (>=)
isOnLeft  = isInCone (<)  skip (>=)
isOnDown  = isInCone skip (>)  (<)
isOnUp    = isInCone skip (<)  (<)

skip :: Double -> Double -> Bool
skip _ _ = True

isInCone :: (Double -> Double -> Bool) -> (Double -> Double -> Bool) -> (Double -> Double -> Bool) -> Position -> ExpressionNode -> Bool
isInCone cmpDXZero cmpDYZero cmpDims pos node = dx `cmpDXZero` 0.0 && dy `cmpDYZero` 0.0 && abs dx `cmpDims` abs dy where
    nodePos = node ^. position
    dx = nodePos ^. x - pos ^. x
    dy = nodePos ^. y - pos ^. y

findNearestNode :: Position -> [ExpressionNode] -> ExpressionNode
findNearestNode pos = minimumBy (compare `on` distance pos)

distance :: Position -> ExpressionNode -> Double
distance pos node = lengthSquared (wpos ^. vector - pos ^. vector) where
    wpos = node ^. position
