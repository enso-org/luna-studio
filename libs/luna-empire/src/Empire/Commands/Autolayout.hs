{-# LANGUAGE MultiWayIf #-}
module Empire.Commands.Autolayout where

import           Control.Monad.State.Lazy (execState, get, gets, modify)
import qualified Control.Monad.State.Lazy as S
import           Data.Foldable            (find)
import qualified Data.List                as List
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Traversable         (forM)
import           Empire.API.Data.Node     (ExpressionNode, NodeId, exprNodeId, position)
import           Empire.API.Data.Port     (isSelf)
import           Empire.API.Data.PortRef  (InPortRef, OutPortRef, dstNodeId, dstPortId, srcNodeId, srcPortId)
import           Empire.API.Data.Position (Position, fromDoubles, leftTopPoint, move, vector, x, y)
import           Empire.API.Data.Vector2  (Vector2)
import           Empire.Prelude

-- This should be changed to Connection from Empire.API.Data.Connection in whole backend
type Connection = (OutPortRef, InPortRef)


data DFSState = NotProcessed | InProcess | Processed deriving (Eq, Show)
data NodeInfo = NodeInfo { _nodeId   :: NodeId
                         , _actPos   :: Position
                         , _subgraph :: Maybe NodeId
                         , _dfsState :: DFSState
                         , _inConns  :: [Connection]
                         , _outConns :: [Connection]
                         } deriving Show
makeLenses ''NodeInfo

data Subgraph = Subgraph { _subgraphId :: NodeId
                         , _members    :: Set NodeId
                         , _yMinMaxAtX :: Map Double (Double, Double)
                         }
makeLenses ''Subgraph

type NodesInfoMap      = Map NodeId NodeInfo
type SubgraphMap       = Map NodeId Subgraph
type AutolayoutState a = S.State NodesInfoMap a



gapBetweenNodes :: Double
gapBetweenNodes = 16 * gridSize


-- Things in this section exist in GUI

gridSize :: Double
gridSize = 16


snapCoord :: Double -> Double
snapCoord p = (* gridSize) . fromIntegral $ (round $ p / gridSize :: Integer)

snap :: Position -> Position
snap = (x %~ snapCoord) . (y %~ snapCoord)

-------



autolayoutNodes :: [NodeId] -> [ExpressionNode] -> [Connection] -> [(NodeId, Position)]
autolayoutNodes nids allNodes allConns =
    let nidsSet   = Set.fromList nids
        nodeInSet = flip Set.member nidsSet . view exprNodeId
        nodes = filter nodeInSet allNodes
        leftTop  = maybe (fromDoubles 0 0) snap $ leftTopPoint $ view position <$> nodes
        nodesMap = Map.fromList $ flip map nodes $ \n ->
            let nid       = n ^. exprNodeId
                inConns'  = filter ((== nid) . view (_2 . dstNodeId)) allConns
                outConns' = filter ((== nid) . view (_1 . srcNodeId)) allConns
            in (nid, NodeInfo nid leftTop Nothing NotProcessed inConns' outConns')
    in Map.toList $ fmap (view actPos) $ execState (findPositions leftTop) nodesMap

clearDFSState :: AutolayoutState ()
clearDFSState = traverse . dfsState .= NotProcessed

findPositions :: Position -> AutolayoutState ()
findPositions pos = do
    removeCycles
    nids <- gets Map.keys
    clearDFSState
    mapM_ findPositionRecursive nids
    mapM_ alignChainsX nids
    clearDFSState
    alignNodesY pos

findPositionRecursive :: NodeId -> AutolayoutState ()
findPositionRecursive nid = withJustM (lookupNode nid) $ \n -> do
    ix nid . dfsState .= Processed
    alignNeighboursX n >>= mapM_ findPositionRecursive

lookupNode :: NodeId -> AutolayoutState (Maybe NodeInfo)
lookupNode nid = gets $ Map.lookup nid

lookupNodes :: [NodeId] -> AutolayoutState [NodeInfo]
lookupNodes = fmap catMaybes . mapM lookupNode

areInChain :: NodeInfo -> NodeInfo -> Bool
areInChain n1 n2 =
    (maybe False (isIdTheSame n2) $ onlyToSelfConnNid n1) ||
    (maybe False (isIdTheSame n1) $ onlyToSelfConnNid n2) ||
    areInChain' n1 n2 ||
    areInChain' n2 n1 where
        isIdTheSame n nid   = nid == n ^. nodeId
        areInChain' n1' n2' = maybe False (\(nid2, nid1) -> isIdTheSame n2' nid2 && isIdTheSame n1' nid1) $ (,) <$> onlyOutConnNid n1' <*> onlyInConnNid n2'
        hasSingleInConn  n  = length (n ^. inConns)  == 1
        hasSingleOutConn n  = length (n ^. outConns) == 1
        onlyInConnNid  n    = if hasSingleInConn  n then Just . view (_1 . srcNodeId) . head $ n ^. inConns  else Nothing
        onlyOutConnNid n    = if hasSingleOutConn n then Just . view (_2 . dstNodeId) . head $ n ^. outConns else Nothing
        onlyToSelfConnNid n = case filter (isSelf . (view $ _2 . dstPortId)) $ n ^. outConns of
            [conn] -> Just $ conn ^. _2 . dstNodeId
            _      -> Nothing

areOnTheSameLevel :: NodeInfo -> NodeInfo -> Bool
areOnTheSameLevel node1 node2 = areOnTheSameLevel' node1 node2 || areOnTheSameLevel' node2 node1 where
    areOnTheSameLevel' n1 n2 = case (sortOutConns $ n1 ^. outConns, sortInConns $ n2 ^. inConns) of
        ([], _)          -> False
        (_, [])          -> False
        (c1 : _, c2 : _) -> c1 == c2

getPrevInChain :: NodeInfo -> AutolayoutState (Maybe NodeInfo)
getPrevInChain n =
    find (areInChain n) . catMaybes <$> mapM (lookupNode . view (_1 . srcNodeId)) (n ^. inConns)

getNextInChain :: NodeInfo -> AutolayoutState (Maybe NodeInfo)
getNextInChain n =
    find (areInChain n) . catMaybes <$> mapM (lookupNode . view (_2 . dstNodeId)) (n ^. outConns)

isHeadInChain :: NodeInfo -> AutolayoutState Bool
isHeadInChain = fmap isNothing . getPrevInChain

isLastInChain :: NodeInfo -> AutolayoutState Bool
isLastInChain = fmap isNothing . getNextInChain


removeCycles :: AutolayoutState ()
removeCycles = get >>= mapM_ removeCyclesForNode . Map.keys

removeCyclesForNode :: NodeId -> AutolayoutState ()
removeCyclesForNode nid = withJustM (lookupNode nid) $ \n -> when (n ^. dfsState == NotProcessed) $ do
    ix nid . dfsState .= InProcess
    let removeConnectionsWithNode :: NodeId -> [Connection] -> [Connection]
        removeConnectionsWithNode nid' = filter (\(src, dst) -> src ^. srcNodeId /= nid && dst ^. dstNodeId /= nid')
        processDstNode :: NodeId -> AutolayoutState ()
        processDstNode dstId = withJustM (lookupNode dstId) $ \dstNode -> case dstNode ^. dfsState of
            NotProcessed -> removeCyclesForNode dstId
            Processed    -> return ()
            InProcess    -> do
                ix dstId . inConns  %= removeConnectionsWithNode nid
                ix nid   . outConns %= removeConnectionsWithNode dstId
    forM_ (view (_2 . dstNodeId) <$> n ^. outConns) $ processDstNode
    ix nid . dfsState .= Processed

alignNeighboursX :: NodeInfo -> AutolayoutState (Set NodeId)
alignNeighboursX n = do
    let prevX = n ^. actPos . x - gapBetweenNodes
        nextX = n ^. actPos . x + gapBetweenNodes
        proccessPred :: NodeInfo -> AutolayoutState (Maybe NodeId)
        proccessPred node = if node ^. dfsState == Processed && node ^. actPos . x <= prevX
            then return Nothing
            else do
                let nid = node ^. nodeId
                ix nid . actPos . x .= prevX
                return $ Just nid
        proccessSucc :: NodeInfo -> AutolayoutState (Maybe NodeId)
        proccessSucc node = if node ^. dfsState == Processed && node ^. actPos . x >= nextX
            then return Nothing
            else do
                let nid = node ^. nodeId
                ix nid . actPos . x .= nextX
                return $ Just nid
    preds <- lookupNodes . map (view srcNodeId . fst) $ n ^. inConns
    succs <- lookupNodes . map (view dstNodeId . snd) $ n ^. outConns
    predsToUpdate <- fmap catMaybes $ mapM proccessPred preds
    succsToUpdate <- fmap catMaybes $ mapM proccessSucc succs
    return . Set.fromList $ predsToUpdate ++ succsToUpdate

alignChainsX :: NodeId -> AutolayoutState ()
alignChainsX nid = withJustM (lookupNode nid) $ \n -> do
    preds  <- lookupNodes . map (view srcNodeId . fst) $ n ^. inConns
    succs  <- lookupNodes . map (view dstNodeId . snd) $ n ^. outConns
    isLast <- isLastInChain n
    let alignToLeft = not (null preds) && (null succs || (length succs == 1 && not isLast))
    if alignToLeft then do
        let maxPredX = maximum $ map (view $ actPos . x) preds
        when (maxPredX < n ^. actPos . x - gapBetweenNodes ) $ do
            ix (n ^. nodeId) . actPos . x .= maxPredX + gapBetweenNodes
            forM_ succs $ alignChainsX . view nodeId
    else if not $ null succs then do
        let minSuccX = minimum $ map (view $ actPos . x) succs
        when (minSuccX > n ^. actPos . x + gapBetweenNodes ) $ do
            ix (n ^. nodeId) . actPos . x .= minSuccX - gapBetweenNodes
            forM_ preds $ alignChainsX . view nodeId
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
    let sortFunction (src1, dst1) (src2, dst2) = compare (dst1 ^. dstPortId) (dst2 ^. dstPortId)
    List.sortBy sortFunction conns

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing  Nothing  = Nothing
maxMaybe (Just a) Nothing  = Just a
maxMaybe Nothing  (Just b) = Just b
maxMaybe (Just a) (Just b) = Just $ max a b

findYPositionForward :: NodeInfo -> Subgraph -> AutolayoutState (Maybe Double)
findYPositionForward n s = do
    succs <- lookupNodes . map (view $ _2 . dstNodeId) . sortOutConns $ n ^. outConns
    let mayNYPos = (+gapBetweenNodes) . snd <$> (Map.lookup (n ^. actPos . x) $ s ^. yMinMaxAtX)
    if null succs || (not . areOnTheSameLevel n $ head succs)
        then return mayNYPos
    else if isJust (view subgraph $ head succs)
        then return . Just . (view $ actPos . y) $ head succs
        else maxMaybe mayNYPos <$> findYPositionForward (head succs) s

findYPositionBackward :: NodeInfo -> Subgraph -> AutolayoutState (Maybe Double)
findYPositionBackward n s = do
    preds <- lookupNodes . map (view $ _1 . srcNodeId) . sortInConns $ n ^. inConns
    let mayNYPos = (+gapBetweenNodes) . snd <$> (Map.lookup (n ^. actPos . x) $ s ^. yMinMaxAtX)
    if null preds || (not . areOnTheSameLevel n $ head preds)
        then return mayNYPos
    else if isJust (view subgraph $ head preds)
        then return . Just . (view $ actPos . y) $ head preds
        else maxMaybe mayNYPos <$> findYPositionBackward (head preds) s

findYPosition :: NodeInfo -> Subgraph -> AutolayoutState (Maybe Double)
findYPosition n s = maxMaybe <$> findYPositionBackward n s <*> findYPositionForward n s

findHigherNodeForward :: NodeInfo -> AutolayoutState (Maybe NodeInfo)
findHigherNodeForward n = do
    succs <- lookupNodes . map (view $ _2 . dstNodeId) . sortOutConns $ n ^. outConns
    if null succs || (isJust . view subgraph $ head succs)
        then return Nothing
    else if areOnTheSameLevel n $ head succs
        then findHigherNodeForward $ head succs
        else return . Just $ head succs

findHigherNodeBackward :: NodeInfo -> AutolayoutState (Maybe NodeInfo)
findHigherNodeBackward n = do
    preds <- lookupNodes . map (view $ _1 . srcNodeId) . sortInConns $ n ^. inConns
    if null preds || (isJust . view subgraph $ head preds)
        then return Nothing
    else if areOnTheSameLevel n $ head preds
        then findHigherNodeBackward $ head preds
        else return . Just $ head preds

findHigherNode :: NodeInfo -> AutolayoutState (Maybe NodeInfo)
findHigherNode n = if n ^. dfsState == InProcess then return Nothing else do
        mayNode <- findHigherNodeBackward n
        case mayNode of
            Just node -> return $ Just node
            Nothing   -> findHigherNodeForward n

-- add consistency to subgraph position map
addToSubgraph :: Subgraph -> NodeInfo -> AutolayoutState Subgraph
addToSubgraph s n = do
    ix (n ^. nodeId) . dfsState .= InProcess
    if isJust $ n ^. subgraph then return s else findHigherNode n >>= \mayN ->
        if isJust mayN then addToSubgraph s $ fromJust mayN else do
            y' <- maybe (n ^. actPos . y) id <$> findYPosition n s
            modify $ Map.update (\n' -> Just $ n' & actPos . y .~ y'
                                                  & subgraph   ?~ s ^. subgraphId
                                                  & dfsState   .~ Processed) (n ^. nodeId)
            let getYMinMaxAtX = maybe (y', y') (\(minY, maxY) -> (min minY y', max maxY y')) $ Map.lookup (n ^. actPos . x) $ s ^. yMinMaxAtX
                updatedS :: Subgraph
                updatedS = s & members    %~ Set.insert (n ^. nodeId)
                             & yMinMaxAtX %~ Map.insert (n ^. actPos . x) getYMinMaxAtX
                proccessN :: Subgraph -> NodeId -> AutolayoutState Subgraph
                proccessN s' nl = lookupNode nl >>= maybe (return s') (addToSubgraph s')
            foldlM proccessN updatedS $ (map (view $ _1 . srcNodeId) . sortInConns  $ n ^. inConns)
                             ++ (map (view $ _2 . dstNodeId) . sortOutConns $ n ^. outConns)

alignSubgraph :: Position -> Subgraph -> AutolayoutState Position
alignSubgraph pos s = case getSubgraphMinimumRectangle s of
    Nothing                     -> return pos
    Just (leftTop, rightBottom) -> do
        moveSubgraph (pos ^. vector - leftTop ^. vector) s
        return $ pos & y %~ (+ (rightBottom ^. y - leftTop ^. y + gapBetweenNodes))

moveSubgraph :: Vector2 Double -> Subgraph -> AutolayoutState ()
moveSubgraph shift s = mapM_ moveNode $ s ^. members where
    moveNode nid = (ix nid . actPos %= move shift)

getSubgraphMinimumRectangle :: Subgraph -> Maybe (Position, Position)
getSubgraphMinimumRectangle s = if Map.null $ s ^. yMinMaxAtX
    then Nothing
    else Just ( fromDoubles (minimum . Map.keys $ s ^. yMinMaxAtX) (minimum . map fst . Map.elems $ s ^. yMinMaxAtX)
              , fromDoubles (maximum . Map.keys $ s ^. yMinMaxAtX) (maximum . map snd . Map.elems $ s ^. yMinMaxAtX) )

alignNodesY :: Position -> AutolayoutState ()
alignNodesY pos = do
    nids <- gets Map.keys
    let makeSubgraph :: NodeInfo -> AutolayoutState Subgraph
        makeSubgraph n = addToSubgraph (Subgraph (n ^. nodeId) def def) n
    subgraphs <- fmap catMaybes . forM nids $ \nid -> lookupNode nid >>= \mayNode ->
        if isNothing mayNode || (isJust . join $ view subgraph <$> mayNode)
            then return Nothing
            else forM mayNode makeSubgraph
    void $ foldlM alignSubgraph pos subgraphs
