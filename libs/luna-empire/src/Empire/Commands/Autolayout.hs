{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Empire.Commands.Autolayout where

import           Control.Monad.State.Lazy  (evalState)
import qualified Control.Monad.State.Lazy  as S
import           Data.Foldable             (find)
import qualified Data.List                 as List
import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Traversable          (forM)
import           Empire.Prelude
import           LunaStudio.Data.Constants (gapBetweenNodes)
import           LunaStudio.Data.Geometry  (snap)
import           LunaStudio.Data.Node      (NodeId)
import           LunaStudio.Data.Port      (isSelf)
import           LunaStudio.Data.PortRef   (InPortRef, OutPortRef, dstNodeId, dstPortId, srcNodeId, srcPortId)
import           LunaStudio.Data.Position  (Position, fromDoubles, leftTopPoint, move, vector, x, y)
import           LunaStudio.Data.Vector2   (Vector2 (Vector2))

-- This should be changed to Connection from LunaStudio.Data.Connection in whole backend
type Connection = (OutPortRef, InPortRef)


type SubgraphId = NodeId
data DFSState = NotProcessed | InProcess | Processed deriving (Eq, Show)
data AutolayoutNode = AutolayoutNode { _nodeId         :: NodeId
                                     , _actPos         :: Position
                                     , _positionInCode :: Int
                                     , _subgraph       :: Maybe SubgraphId
                                     , _dfsState       :: DFSState
                                     , _inConns        :: [Connection]
                                     , _outConns       :: [Connection]
                                     } deriving Show
makeLenses ''AutolayoutNode



data Subgraph = Subgraph { _subgraphId            :: SubgraphId
                         , _firstAppearanceInCode :: Int
                         , _members               :: Set NodeId
                         } deriving Show

makeLenses ''Subgraph

data AutolayoutState = AutolayoutState { _varPosNodes   :: Map NodeId AutolayoutNode
                                       , _constPosNodes :: Map NodeId Position
                                       , _connections   :: [Connection]
                                       , _subgraphs     :: Map SubgraphId Subgraph
                                       } deriving Show

makeLenses ''AutolayoutState

autolayoutNodes :: [NodeId] -> [(NodeId, Int, Position)] -> [Connection] -> [(NodeId, Position)]
autolayoutNodes nids allNodes allConns = evalState (findPositions leftTop) (AutolayoutState nodesMap (Map.fromList constPosN) allConns mempty) where
    nidsSet   = Set.fromList nids
    nodeInSet = flip Set.member nidsSet . view _1
    (nodes, constPosN') = List.partition nodeInSet allNodes
    constPosN = map (\(nid, _, pos) -> (nid, pos)) constPosN'
    leftTop   = maybe (fromDoubles 0 0) snap $ leftTopPoint $ view _3 <$> nodes
    nodesMap  = Map.fromList $ flip map nodes $ \n ->
        let nid       = n ^. _1
            posInCode = n ^. _2
            inConns'  = filter ((== nid) . view (_2 . dstNodeId)) allConns
            outConns' = filter ((== nid) . view (_1 . srcNodeId)) allConns
        in (nid, AutolayoutNode nid leftTop posInCode def NotProcessed inConns' outConns')


clearDFSState :: S.State AutolayoutState ()
clearDFSState = varPosNodes . traverse . dfsState .= NotProcessed

findPositions :: Position -> S.State AutolayoutState [(NodeId, Position)]
findPositions startPos = do
    nids <- use $ varPosNodes . to Map.keys
    removeCycles
    clearDFSState
    mapM_ findPositionRecursive nids
    mapM_ alignChainsX nids
    clearDFSState
    alignNodesY startPos
    placeSubgraphs


findPositionRecursive :: NodeId -> S.State AutolayoutState ()
findPositionRecursive nid = withJustM (lookupNode nid) $ \n -> do
    varPosNodes . ix nid . dfsState .= Processed
    alignNeighboursX n >>= mapM_ findPositionRecursive

lookupNode :: NodeId -> S.State AutolayoutState (Maybe AutolayoutNode)
lookupNode nid = preuse $ varPosNodes . ix nid

lookupNodes :: [NodeId] -> S.State AutolayoutState [AutolayoutNode]
lookupNodes = fmap catMaybes . mapM lookupNode

areInChain :: AutolayoutNode -> AutolayoutNode -> Bool
areInChain n1 n2 =
    (maybe False (isIdTheSame n2) $ onlyToSelfConnNid n1) ||
    (maybe False (isIdTheSame n1) $ onlyToSelfConnNid n2) ||
    areInChain' n1 n2 ||
    areInChain' n2 n1 where
        isIdTheSame n nid   = nid == n ^. nodeId
        areInChain' n1' n2' = maybe False (\(nid2, nid1) -> isIdTheSame n2' nid2 && isIdTheSame n1' nid1) $ (,) <$> onlyOutConnNid n1' <*> onlyInConnNid n2'
        hasSingleInConn  n  = length (n ^. inConns)  == 1
        hasSingleOutConn n  = length (n ^. outConns) == 1
        onlyInConnNid  n    = if hasSingleInConn  n then Just . view (_1 . srcNodeId) . unsafeHead $ n ^. inConns  else Nothing
        onlyOutConnNid n    = if hasSingleOutConn n then Just . view (_2 . dstNodeId) . unsafeHead $ n ^. outConns else Nothing
        onlyToSelfConnNid n = case filter (isSelf . (view $ _2 . dstPortId)) $ n ^. outConns of
            [conn] -> Just $ conn ^. _2 . dstNodeId
            _      -> Nothing

areOnTheSameLevel :: AutolayoutNode -> AutolayoutNode -> Bool
areOnTheSameLevel node1 node2 = areOnTheSameLevel' node1 node2 || areOnTheSameLevel' node2 node1 where
    areOnTheSameLevel' n1 n2 = case (sortOutConns $ n1 ^. outConns, sortInConns $ n2 ^. inConns) of
        ([], _)          -> False
        (_, [])          -> False
        (c1 : _, c2 : _) -> c1 == c2

getPrevInChain :: AutolayoutNode -> S.State AutolayoutState (Maybe AutolayoutNode)
getPrevInChain n =
    find (areInChain n) . catMaybes <$> mapM (lookupNode . view (_1 . srcNodeId)) (n ^. inConns)

getNextInChain :: AutolayoutNode -> S.State AutolayoutState (Maybe AutolayoutNode)
getNextInChain n =
    find (areInChain n) . catMaybes <$> mapM (lookupNode . view (_2 . dstNodeId)) (n ^. outConns)

isHeadInChain :: AutolayoutNode -> S.State AutolayoutState Bool
isHeadInChain = fmap isNothing . getPrevInChain

isLastInChain :: AutolayoutNode -> S.State AutolayoutState Bool
isLastInChain = fmap isNothing . getNextInChain


removeCycles :: S.State AutolayoutState ()
removeCycles = use varPosNodes >>= mapM_ removeCyclesForNode . Map.keys

removeCyclesForNode :: NodeId -> S.State AutolayoutState ()
removeCyclesForNode nid = withJustM (lookupNode nid) $ \n -> when (n ^. dfsState == NotProcessed) $ do
    varPosNodes . ix nid . dfsState .= InProcess
    let removeConnectionsWithNode :: NodeId -> [Connection] -> [Connection]
        removeConnectionsWithNode nid' = filter (\(src, dst) -> src ^. srcNodeId /= nid && dst ^. dstNodeId /= nid')
        processDstNode :: NodeId -> S.State AutolayoutState ()
        processDstNode dstId = withJustM (lookupNode dstId) $ \dstNode -> case dstNode ^. dfsState of
            NotProcessed -> removeCyclesForNode dstId
            Processed    -> return ()
            InProcess    -> do
                varPosNodes . at dstId . _Just . inConns  %= removeConnectionsWithNode nid
                varPosNodes . at nid   . _Just . outConns %= removeConnectionsWithNode dstId
    for_ (view (_2 . dstNodeId) <$> n ^. outConns) $ processDstNode
    varPosNodes . ix nid . dfsState .= Processed

alignNeighboursX :: AutolayoutNode -> S.State AutolayoutState (Set NodeId)
alignNeighboursX n = do
    let prevX = n ^. actPos . x - gapBetweenNodes
        nextX = n ^. actPos . x + gapBetweenNodes
        proccessPred :: AutolayoutNode -> S.State AutolayoutState (Maybe NodeId)
        proccessPred node = if node ^. dfsState == Processed && node ^. actPos . x <= prevX
            then return Nothing
            else do
                let nid = node ^. nodeId
                varPosNodes . ix nid . actPos . x .= prevX
                return $ Just nid
        proccessSucc :: AutolayoutNode -> S.State AutolayoutState (Maybe NodeId)
        proccessSucc node = if node ^. dfsState == Processed && node ^. actPos . x >= nextX
            then return Nothing
            else do
                let nid = node ^. nodeId
                varPosNodes . ix nid . actPos . x .= nextX
                return $ Just nid
    preds <- lookupNodes . map (view srcNodeId . fst) $ n ^. inConns
    succs <- lookupNodes . map (view dstNodeId . snd) $ n ^. outConns
    predsToUpdate <- fmap catMaybes $ mapM proccessPred preds
    succsToUpdate <- fmap catMaybes $ mapM proccessSucc succs
    return . Set.fromList $ predsToUpdate <> succsToUpdate

alignChainsX :: NodeId -> S.State AutolayoutState ()
alignChainsX nid = withJustM (lookupNode nid) $ \n -> do
    preds  <- lookupNodes . map (view srcNodeId . fst) $ n ^. inConns
    succs  <- lookupNodes . map (view dstNodeId . snd) $ n ^. outConns
    isLast <- isLastInChain n
    let alignToLeft = not (null preds) && (null succs || (length succs == 1 && not isLast))
    if alignToLeft then do
        let maxPredX = unsafeMaximum $ map (view $ actPos . x) preds
        when (maxPredX < n ^. actPos . x - gapBetweenNodes ) $ do
            varPosNodes . ix (n ^. nodeId) . actPos . x .= maxPredX + gapBetweenNodes
            for_ succs $ alignChainsX . view nodeId
    else if not $ null succs then do
        let minSuccX = unsafeMinimum $ map (view $ actPos . x) succs
        when (minSuccX > n ^. actPos . x + gapBetweenNodes ) $ do
            varPosNodes . ix (n ^. nodeId) . actPos . x .= minSuccX - gapBetweenNodes
            for_ preds $ alignChainsX . view nodeId
    else return ()

sortOutConns :: [Connection] -> [Connection]
sortOutConns conns = do
    let sortFunction (src1, dst1) (src2, dst2) =
            if      isSelf $ dst1 ^. dstPortId then LT
            else if isSelf $ dst2 ^. dstPortId then GT
            else (src1 ^. srcPortId) `compare` (src2 ^. srcPortId)
    List.sortBy sortFunction conns

sortInConns :: [Connection] -> [Connection]
sortInConns conns = do
    let sortFunction (_src1, dst1) (_src2, dst2) = compare (dst1 ^. dstPortId) (dst2 ^. dstPortId)
    List.sortBy sortFunction conns

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing  Nothing  = Nothing
maxMaybe (Just a) Nothing  = Just a
maxMaybe Nothing  (Just b) = Just b
maxMaybe (Just a) (Just b) = Just $ max a b

yMinMaxAtX :: [(NodeId, Position)] -> Map Double (Double, Double)
yMinMaxAtX nodes = foldl updateMap def nodes where
    updateValue (miny, maxy) (y', _) = (min miny y', max maxy y')
    updateMap minMaxMap (_, npos) = Map.insertWith updateValue (npos ^. x) (npos ^. y, npos ^. y) minMaxMap


findYPositionForward :: AutolayoutNode -> SubgraphId -> S.State AutolayoutState (Maybe Double)
findYPositionForward n sid = do
    succs <- lookupNodes . map (view $ _2 . dstNodeId) . sortOutConns $ n ^. outConns
    nids <- fromMaybe def <$> preuse (subgraphs . ix sid . members)
    nodes <- Map.toList . fmap (view actPos) . flip Map.restrictKeys nids <$> use varPosNodes
    let minMaxMap = yMinMaxAtX nodes
        mayNYPos = (+gapBetweenNodes) . snd <$> (Map.lookup (n ^. actPos . x) minMaxMap)
    if null succs || (not . areOnTheSameLevel n $ unsafeHead succs)
        then return mayNYPos
    else if isJust (view subgraph $ unsafeHead succs)
        then return . maxMaybe mayNYPos $ Just . (view $ actPos . y) $ unsafeHead succs
        else maxMaybe mayNYPos <$> findYPositionForward (unsafeHead succs) sid

findYPositionBackward :: AutolayoutNode -> SubgraphId -> S.State AutolayoutState (Maybe Double)
findYPositionBackward n sid = do
    preds <- lookupNodes . map (view $ _1 . srcNodeId) . sortInConns $ n ^. inConns
    nids <- fromMaybe def <$> preuse (subgraphs . ix sid . members)
    nodes <- Map.toList . fmap (view actPos) . flip Map.restrictKeys nids <$> use varPosNodes
    let minMaxMap = yMinMaxAtX nodes
        mayNYPos = (+gapBetweenNodes) . snd <$> Map.lookup (n ^. actPos . x) minMaxMap
    if null preds || (not . areOnTheSameLevel n $ unsafeHead preds)
        then return mayNYPos
    else if isJust (view subgraph $ unsafeHead preds)
        then return . maxMaybe mayNYPos $ Just . (view $ actPos . y) $ unsafeHead preds
        else maxMaybe mayNYPos <$> findYPositionBackward (unsafeHead preds) sid

findYPosition :: AutolayoutNode -> SubgraphId -> S.State AutolayoutState (Maybe Double)
findYPosition n sid = maxMaybe <$> findYPositionBackward n sid <*> findYPositionForward n sid

findHigherNodeForward :: AutolayoutNode -> S.State AutolayoutState (Maybe AutolayoutNode)
findHigherNodeForward n = do
    succs <- lookupNodes . map (view $ _2 . dstNodeId) . sortOutConns $ n ^. outConns
    if null succs || (isJust . view subgraph $ unsafeHead succs)
        then return Nothing
    else if areOnTheSameLevel n $ unsafeHead succs
        then findHigherNodeForward $ unsafeHead succs
        else return . Just $ unsafeHead succs

findHigherNodeBackward :: AutolayoutNode -> S.State AutolayoutState (Maybe AutolayoutNode)
findHigherNodeBackward n = do
    preds <- lookupNodes . map (view $ _1 . srcNodeId) . sortInConns $ n ^. inConns
    if null preds || (isJust . view subgraph $ unsafeHead preds)
        then return Nothing
    else if areOnTheSameLevel n $ unsafeHead preds
        then findHigherNodeBackward $ unsafeHead preds
        else return . Just $ unsafeHead preds

findHigherNode :: AutolayoutNode -> S.State AutolayoutState (Maybe AutolayoutNode)
findHigherNode n = if n ^. dfsState == InProcess then return Nothing else do
        mayNode <- findHigherNodeBackward n
        case mayNode of
            Just node -> return $ Just node
            Nothing   -> findHigherNodeForward n

findSubgraph :: SubgraphId -> NodeId -> S.State AutolayoutState ()
findSubgraph sid nid = withJustM (lookupNode nid) $ \n -> do
    let addToSubgraph node = do
            y' <- maybe (node ^. actPos . y) id <$> findYPosition node sid
            varPosNodes . ix nid %= \n' -> n' & actPos . y .~ y'
                                              & subgraph   ?~ sid
                                              & dfsState   .~ Processed
            subgraphs . ix sid . members %= Set.insert nid
            subgraphs . ix sid . firstAppearanceInCode %= min (n ^. positionInCode)
            mapM_ (findSubgraph sid) $ (map (view $ _1 . srcNodeId) . sortInConns  $ node ^. inConns)
                                    <> (map (view $ _2 . dstNodeId) . sortOutConns $ node ^. outConns)

    varPosNodes . ix nid . dfsState .= InProcess
    unless (isJust $ n ^. subgraph) $ findHigherNode n >>= maybe (addToSubgraph n) (findSubgraph sid . view nodeId)

alignSubgraph :: Position -> SubgraphId -> S.State AutolayoutState Position
alignSubgraph pos sid = getSubgraphMinimumRectangle sid >>= \rect -> case rect of
    Nothing                     -> return pos
    Just (leftTop, rightBottom) -> do
        moveSubgraph (pos ^. vector - leftTop ^. vector) sid
        return $ pos & y %~ (+ (rightBottom ^. y - leftTop ^. y + gapBetweenNodes))

moveSubgraph :: Vector2 Double -> SubgraphId -> S.State AutolayoutState ()
moveSubgraph shift sid = do
    let moveNode nid = varPosNodes . ix nid . actPos %= move shift
    nids <- use $ subgraphs . ix sid . members . to Set.toList
    mapM_ moveNode nids

getSubgraphMinimumRectangle :: SubgraphId -> S.State AutolayoutState (Maybe (Position, Position))
getSubgraphMinimumRectangle sid = do
    nids <- Set.toList <$> (use $ subgraphs . ix sid . members)
    positions <- map (view actPos) <$> lookupNodes nids
    let mergeWithResult :: Maybe (Position, Position) -> Position -> Maybe (Position, Position)
        mergeWithResult Nothing pos = Just (pos, pos)
        mergeWithResult (Just (minPos, maxPos)) pos = do
            let minx = min (minPos ^. x) (pos ^. x)
                miny = min (minPos ^. y) (pos ^. y)
                maxx = max (maxPos ^. x) (pos ^. x)
                maxy = max (maxPos ^. y) (pos ^. y)
            Just (fromDoubles minx miny, fromDoubles maxx maxy)
    return $ foldl mergeWithResult Nothing positions

alignNodesY :: Position -> S.State AutolayoutState ()
alignNodesY pos = do
    nids <- use $ varPosNodes . to Map.keys
    let makeSubgraph :: NodeId -> S.State AutolayoutState ()
        makeSubgraph nid = do
            subgraphs . at nid ?= Subgraph nid maxBound def
            findSubgraph nid nid
    for_ nids $ \nid -> withJustM (lookupNode nid) $ \n ->
        when (isNothing $ n ^. subgraph) $ makeSubgraph nid
    use (subgraphs . to Map.keys) >>= void . foldlM alignSubgraph pos

acceptSubgraph :: SubgraphId -> S.State AutolayoutState [(NodeId, Position)]
acceptSubgraph sid = withJustM (preuse $ subgraphs . ix sid . members . to Set.toList) $ \nids -> do
    subgraphs . at sid .= Nothing
    let changeState n = do
            let nid = n ^. nodeId
            constPosNodes . at nid ?= n ^. actPos
            varPosNodes   . at nid .= Nothing
            return $ Just (nid, n ^. actPos)
        acceptNode nid = lookupNode nid >>= maybe (return Nothing) changeState
    catMaybes <$> mapM acceptNode nids

placeSubgraphs :: S.State AutolayoutState [(NodeId, Position)]
placeSubgraphs = do
    sids <- map (view subgraphId) . List.sortOn (view firstAppearanceInCode) <$> use (subgraphs . to Map.elems)
    fmap concat . forM sids $ \sid -> do
        alignToEndpoint sid
        meetTheConstraints sid
        acceptSubgraph sid

lookupMax :: Map k a -> Maybe (k, a)
lookupMax m = if Map.null m then Nothing else Just $ Map.findMax m

alignToEndpoint :: SubgraphId -> S.State AutolayoutState ()
alignToEndpoint sid = do
    nids <- fromMaybe def <$> preuse (subgraphs . ix sid . members)
    nodes <- use varPosNodes
    constPosNodes' <- use constPosNodes
    let constPosId = Map.keysSet constPosNodes'
        isSrcEndConn (src, dst) = Set.member (src ^. srcNodeId) constPosId && Set.member (dst ^. dstNodeId) nids
        isDstEndConn (src, dst) = Set.member (src ^. srcNodeId) nids && Set.member (dst ^. dstNodeId) constPosId
        isEndConn conn = isSrcEndConn conn || isDstEndConn conn
        processInConn :: Maybe (Vector2 Double) -> Connection -> Maybe (Vector2 Double)
        processInConn res (src, dst) = do
            case Map.lookup (src ^. srcNodeId) constPosNodes' of
                Nothing  -> res
                Just pos -> case view actPos <$> Map.lookup (dst ^. dstNodeId) nodes of
                    Nothing -> res
                    Just oldPos -> do
                        let shift = findSuccessorPosition (src ^. srcNodeId, pos) (Map.toList constPosNodes') ^. vector - oldPos ^. vector
                        if isNothing res || Just (shift ^. x) > (view x <$> res) then Just shift else res
        processOutConn :: Maybe (Vector2 Double) -> Connection -> Maybe (Vector2 Double)
        processOutConn res (src, dst) = do
            case Map.lookup (dst ^. dstNodeId) constPosNodes' of
                Nothing  -> res
                Just pos -> case view actPos <$> Map.lookup (src ^. srcNodeId) nodes of
                    Nothing -> res
                    Just oldPos -> do
                        let shift = findSuccessorPosition (dst ^. dstNodeId, pos) (Map.toList constPosNodes') ^. vector - oldPos ^. vector
                        if isNothing res || Just (shift ^. x) < (view x <$> res) then Just shift else res
    (sInConns, sOutConns) <- List.partition isSrcEndConn . filter isEndConn <$> use connections
    let mayInShift = foldl processInConn Nothing sInConns
        mayOutShift = foldl processOutConn Nothing sOutConns
    case (mayInShift, mayOutShift) of
        (Just inShift, _)  -> moveSubgraph inShift sid
        (_, Just outShift) -> moveSubgraph outShift sid
        _ -> return ()

findPredecessorPosition :: (NodeId, Position) -> [(NodeId, Position)] -> Position
findPredecessorPosition node nodes = fromDoubles xPos yPos where
    xPos = (node ^. _2 . x) - gapBetweenNodes
    yPos = findYPos $ node ^. _2 . y
    findYPos y' = if any (\n -> n ^. _2 . x == xPos && n ^. _2 . y == y') nodes then findYPos $ y' - gapBetweenNodes else y'

findSuccessorPosition :: (NodeId, Position) -> [(NodeId, Position)] -> Position
findSuccessorPosition node nodes = fromDoubles xPos yPos where
    xPos = (node ^. _2 . x) + gapBetweenNodes
    yPos = findYPos $ node ^. _2 . y
    findYPos y' = if any (\n -> n ^. _2 . x == xPos && n ^. _2 . y == y') nodes then findYPos $ y' + gapBetweenNodes else y'




meetTheConstraints :: SubgraphId -> S.State AutolayoutState ()
meetTheConstraints sid = do
    nids <- fromMaybe def <$> preuse (subgraphs . ix sid . members)
    constPNMap <- use constPosNodes
    conns <- use connections
    nodes <- Map.toList . fmap (view actPos) . flip Map.restrictKeys nids <$> use varPosNodes
    let constYMinMaxAtX :: Map Double (Double, Double)
        constYMinMaxAtX = yMinMaxAtX $ Map.toList constPNMap
        checkNodeConstraints :: Map Double (Double, Double) -> (NodeId, Position) -> Bool
        checkNodeConstraints minMaxMap (_nid, pos) = do
            let meetsConstraint (miny, maxy) = pos ^. y <= miny - gapBetweenNodes || pos ^. y >= maxy + gapBetweenNodes
            all meetsConstraint . Map.elems $ Map.filterWithKey (\mx _ -> abs (pos ^. x - mx) < gapBetweenNodes) minMaxMap
        neededShiftY' :: Map Double (Double, Double) -> (NodeId, Position) -> Double
        neededShiftY' minMaxMap (_nid, pos) = do
            let minimalShiftY :: (Double, Double) -> Double
                minimalShiftY (_, maxy) = maxy + gapBetweenNodes - pos ^. y
                neededShifts :: [Double]
                neededShifts = map minimalShiftY . Map.elems $ Map.filterWithKey (\mx _ -> abs (pos ^. x - mx) < gapBetweenNodes) minMaxMap
            foldl max 0 neededShifts
        neededShiftY :: Map Double (Double, Double) -> [(NodeId, Position)] -> Double
        neededShiftY minMaxMap nodes' = foldl (\res -> max res . neededShiftY' minMaxMap) 0 nodes'
        alignDownIfNeeded :: S.State AutolayoutState ()
        alignDownIfNeeded = if all (checkNodeConstraints constYMinMaxAtX) nodes then return ()
                            else let shift = Vector2 0 $ neededShiftY constYMinMaxAtX nodes in
                                if shift == 0 then return () else do
                                    moveSubgraph shift sid
        alignRight :: S.State AutolayoutState ()
        alignRight = do
            let findSuccPos Nothing = Nothing
                findSuccPos (Just (px, (py, _))) = Just $ fromDoubles (px + gapBetweenNodes) py
                mayNewPos = findSuccPos $ lookupMax constYMinMaxAtX
                foldFunc Nothing (_, pos) = Just pos
                foldFunc (Just res) (_, pos) = if res ^. x < pos ^. x then Just res
                    else if res ^. x == pos ^. x && res ^. y < pos ^. y then Just res
                    else Just pos
                mayOldPos = foldl foldFunc Nothing nodes
                findShift oldPos newPos = newPos ^. vector - oldPos ^. vector
            withJust ((,) <$> mayOldPos <*> mayNewPos) $ \(oldPos, newPos) -> moveSubgraph (findShift oldPos newPos) sid
    let isForeignConn (src, dst) = if      Set.member (src ^. srcNodeId) nids && Map.member (dst ^. dstNodeId) constPNMap then True
                                   else if Map.member (src ^. srcNodeId) constPNMap && Set.member (dst ^. dstNodeId) nids then True
                                   else False
    if any isForeignConn conns then alignDownIfNeeded else alignRight
