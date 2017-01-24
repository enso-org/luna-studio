module Luna.Studio.Handler.Navigation where

import qualified Data.HashMap.Strict                as HashMap

import           Data.Position                      (Position (Position), lengthSquared, magnitude, vector, x, y)
import           Luna.Studio.Prelude

import qualified Empire.API.Data.Connection         as C
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Port               as P
import qualified Empire.API.Data.PortRef            as R
import           Luna.Studio.Action.Batch           (cancelCollaborativeTouch, collaborativeTouch)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph           (allNodes)
import           Luna.Studio.Event.Event            (Event (Shortcut))
import           Luna.Studio.Event.Shortcut         (ShortcutEvent (..))
import           Luna.Studio.React.Model.Node       (Node)
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph



toAction :: Event -> Maybe (Command State ())
toAction (Shortcut shortcut) = Just $ handleShortcut shortcut
toAction _ = Nothing

handleShortcut :: ShortcutEvent -> Command State ()
handleShortcut = \case
    GoConeDown  -> goConeDown
    GoConeLeft  -> goConeLeft
    GoConeRight -> goConeRight
    GoConeUp    -> goConeUp
    GoDown      -> goDown
    GoLeft      -> goLeft
    GoNext      -> goNext
    GoPrev      -> goPrev
    GoRight     -> goRight
    GoUp        -> goUp
    _           -> return ()

goPrev :: Command State ()
goPrev = do
    nodes <- allNodes
    let selectedNodes = findSelected nodes
        selectedIds   = view Model.nodeId <$> selectedNodes
    unless (null selectedNodes) $ do
        let nodeSrc = findLeftMost selectedNodes
            nodeId = nodeSrc ^. Model.nodeId
            inPortRefSelf      = R.InPortRef nodeId P.Self
            inPortRefFirstPort = R.InPortRef nodeId $ P.Arg 0
        prevSelfNodeIdMay <- preuse $ Global.graph . Graph.connectionsMap . ix inPortRefSelf . C.src . R.srcNodeId
        case prevSelfNodeIdMay of
            Just prevSelfNodeId -> goToNodeId selectedIds prevSelfNodeId
            Nothing -> do
                prevFirstPortNodeIdMay <- preuse $ Global.graph . Graph.connectionsMap . ix inPortRefFirstPort . C.src . R.srcNodeId
                withJust prevFirstPortNodeIdMay $ goToNodeId selectedIds

goNext :: Command State ()
goNext = do
    nodes <- allNodes
    let selectedNodes = findSelected nodes
    unless (null selectedNodes) $ do
        let nodeSrc = findRightMost selectedNodes
            nodeId = nodeSrc ^. Model.nodeId
        nextNodeIds <- getDstNodeIds nodeId
        nextNodes <- catMaybes <$> mapM Global.getNode nextNodeIds
        unless (null nextNodes) $ do
            let nextNode = findUpMost nextNodes
            changeSelection (view Model.nodeId <$> selectedNodes) $ nextNode ^. Model.nodeId

getDstNodeIds :: NodeId -> Command State [NodeId]
getDstNodeIds nodeId = do
    connMap <- use $ Global.graph . Graph.connectionsMap
    let connections = filter matchNodeId $ HashMap.elems connMap
    return $ (^. C.dst . R.dstNodeId) <$> connections
    where
        matchNodeId conn = conn ^. C.src . R.srcNodeId == nodeId

goToNodeId :: [NodeId] -> NodeId -> Command State ()
goToNodeId = changeSelection

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
    nodes <- allNodes
    let selectedNodes = findSelected nodes
    unless (null selectedNodes) $ do
        let nodeSrc = findMost selectedNodes
            pos = nodeSrc ^. Model.position
            nodesSide = findNodesOnSide pos nodes
        unless (null nodesSide) $ do
            let nearest = findNearest pos nodesSide ^. Model.nodeId
            changeSelection (view Model.nodeId <$> selectedNodes) nearest

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
    pos' = node ^. Model.position
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
    nodes <- allNodes
    let selectedNodes = findSelected nodes
        selectedIds   = view Model.nodeId <$> selectedNodes
    unless (null selectedNodes) $ do
        let nodeSrc = findMost selectedNodes
            pos = nodeSrc ^. Model.position
            nodesCone = findNodesInCone pos nodes
            nodesSide = findNodesOnSide pos nodes
        if not $ null nodesCone
            then                           changeSelection selectedIds $ findNearestNode pos nodesCone ^. Model.nodeId
            else unless (null nodesSide) $ changeSelection selectedIds $ findNearestNode pos nodesSide ^. Model.nodeId

findRightMost, findLeftMost, findDownMost, findUpMost :: [Node] -> Node
findRightMost = maximumBy (compare `on` (^. Model.position . x))
findLeftMost  = minimumBy (compare `on` (^. Model.position . x))
findDownMost  = maximumBy (compare `on` (^. Model.position . y))
findUpMost    = minimumBy (compare `on` (^. Model.position . y))

findNodesOnRightSide, findNodesOnLeftSide, findNodesOnDownSide, findNodesOnUpSide :: Position -> [Node] -> [Node]
findNodesOnRightSide pos = filter $ \node -> node ^. Model.position . x > pos ^. x
findNodesOnLeftSide  pos = filter $ \node -> node ^. Model.position . x < pos ^. x
findNodesOnDownSide  pos = filter $ \node -> node ^. Model.position . y > pos ^. y
findNodesOnUpSide    pos = filter $ \node -> node ^. Model.position . y < pos ^. y

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
    nodePos = node ^. Model.position
    dx = nodePos ^. x - pos ^. x
    dy = nodePos ^. y - pos ^. y

findSelected :: [Node] -> [Node]
findSelected = filter $ view Node.isSelected

findNearestNode :: Position -> [Node] -> Node
findNearestNode pos = minimumBy (compare `on` distance pos)

distance :: Position -> Node -> Double
distance pos node = lengthSquared (wpos ^. vector - pos ^. vector) where
    wpos = node ^. Model.position

changeSelection :: [NodeId] -> NodeId -> Command State ()
changeSelection selectedNodes node = do
    unselectNodes selectedNodes
    selectNode node

unselectNodes :: [NodeId] -> Command State ()
unselectNodes selectedNodeIds = do
    Global.modifyNodeEditor $ forM_ selectedNodeIds $ \nodeId ->
        NodeEditor.nodes . at nodeId %= fmap (Model.isSelected .~ False)
    cancelCollaborativeTouch selectedNodeIds

selectNode :: NodeId -> Command State ()
selectNode nodeId = do
    Global.modifyNode nodeId $ Model.isSelected .= True
    collaborativeTouch [nodeId]
