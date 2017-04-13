{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.Autolayout where

import           Control.Monad.State.Lazy                    (execStateT, get, modify)
import qualified Control.Monad.State.Lazy                    as S
import           Data.Map.Lazy                               (Map)
import qualified Data.Map.Lazy                               as Map
import           Data.Position                               (Position, fromDoubles, minimumRectangle, move, vector, x, y)
import           Data.Set                                    (Set)
import qualified Data.Set                                    as Set
import           Data.Vector                                 (Vector2)
import           Luna.Studio.Action.Basic                    (moveNodes)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getConnectionsFromNode, getConnectionsToNode, getExpressionNodes,
                                                              getSelectedNodes)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection          (Connection, dstNodeLoc, dstPortId, srcNodeLoc, srcPortId)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as Node
import           Luna.Studio.React.Model.Port                (InPort (Self), PortId (InPortId), isSelf)
import           Luna.Studio.State.Global                    (State)


data DFSState = NotProccessed | InProccess | Proccessed deriving (Eq, Show)
data NodeInfo = NodeInfo { _nodeLoc    :: NodeLoc
                         , _name       :: Text
                         , _actPos     :: Position
                         , _dfsState   :: DFSState
                         , _subgraphId :: Maybe NodeLoc
                         , _inConns    :: [Connection]
                         , _outConns   :: [Connection]
                         }
makeLenses ''NodeInfo

data Subgraph = Subgraph { _sid      :: NodeLoc
                         , _nodeLocs :: Set NodeLoc
                         , _shape    :: Map Double (Double, Double)
                         }
makeLenses ''Subgraph

type NodesInfoMap      = Map NodeLoc NodeInfo
type SubgraphMap       = Map NodeLoc Subgraph
type AutolayoutState a = S.StateT NodesInfoMap IO a


gapBetweenNodes :: Double
gapBetweenNodes = 150

autolayoutAllNodes :: Command State ()
autolayoutAllNodes = getExpressionNodes >>= autolayoutNodes

autolayoutSelectedNodes :: Command State ()
autolayoutSelectedNodes = getSelectedNodes >>= autolayoutNodes

autolayoutNodes :: [ExpressionNode] -> Command State ()
autolayoutNodes nodes = do
    let mayMinRect = minimumRectangle $ map (view Node.position) nodes
    withJust mayMinRect $ \(leftTop, _) -> do
        nodesMap <- fmap Map.fromList $ forM nodes $ \n -> do
            let nl = n ^. Node.nodeLoc
            inConns'  <- getConnectionsToNode   nl
            outConns' <- getConnectionsFromNode nl
            return $ (nl, NodeInfo nl
                                   (n ^. Node.name)
                                   leftTop
                                   NotProccessed
                                   Nothing
                                   inConns'
                                   outConns' )
        ns <- liftIO $ execStateT (findPositions leftTop) nodesMap
        moveNodes . Map.toList . fmap (view actPos) $ ns

clearDFSState :: AutolayoutState ()
clearDFSState = modify $ fmap (\n -> n & dfsState .~ NotProccessed)

findPositions :: Position -> AutolayoutState ()
findPositions pos = do
    removeCycles
    nls <- Map.keys <$> get
    mapM_ findPositionRecursive nls
    mapM_ alignChainsX nls
    alignNodesY pos


findPositionRecursive :: NodeLoc -> AutolayoutState ()
findPositionRecursive nl = lookupNode nl >>= \mayNode -> case mayNode of
    Nothing -> return ()
    Just n  -> alignNeighbours n >>= mapM_ findPositionRecursive

lookupNode :: NodeLoc -> AutolayoutState (Maybe NodeInfo)
lookupNode nl = Map.lookup nl <$> get

lookupNodes :: [NodeLoc] -> AutolayoutState [NodeInfo]
lookupNodes = fmap catMaybes . mapM lookupNode

areInChain :: NodeInfo -> NodeInfo -> Bool
areInChain n1 n2 =
    (maybe False (isNlTheSame n2) $ onlyToSelfConnNl n1) ||
    (maybe False (isNlTheSame n1) $ onlyToSelfConnNl n2) ||
    areInChain' n1 n2 ||
    areInChain' n2 n1 where
        isNlTheSame n nl    = nl == n ^. nodeLoc
        areInChain' n1' n2' = maybe False (\(nl2, nl1) -> isNlTheSame n2' nl2 && isNlTheSame n1' nl1) $ (,) <$> onlyOutConnNl n1' <*> onlyInConnNl n2'
        hasSingleInConn  n  = length (n ^. inConns)  == 1
        hasSingleOutConn n  = length (n ^. outConns) == 1
        onlyInConnNl  n     = if hasSingleInConn  n then Just . view srcNodeLoc . head $ n ^. inConns  else Nothing
        onlyOutConnNl n     = if hasSingleOutConn n then Just . view dstNodeLoc . head $ n ^. outConns else Nothing
        onlyToSelfConnNl n  = case filter (isSelf . InPortId . view dstPortId) $ n ^. outConns of
            [conn] -> Just $ conn ^. dstNodeLoc
            _      -> Nothing

areOnTheSameLevel :: NodeInfo -> NodeInfo -> Bool
areOnTheSameLevel node1 node2 = areOnTheSameLevel' node1 node2 || areOnTheSameLevel' node2 node1 where
    areOnTheSameLevel' n1 n2 = case (sortOutConns $ n1 ^. outConns, sortInConns $ n2 ^. inConns) of
        ([], _)          -> False
        (_, [])          -> False
        (c1 : _, c2 : _) -> c1 == c2

getPrevInChain :: NodeInfo -> AutolayoutState (Maybe NodeInfo)
getPrevInChain n =
    find (areInChain n) . catMaybes <$> mapM (lookupNode . view srcNodeLoc) (n ^. inConns)

getNextInChain :: NodeInfo -> AutolayoutState (Maybe NodeInfo)
getNextInChain n =
    find (areInChain n) . catMaybes <$> mapM (lookupNode . view dstNodeLoc) (n ^. outConns)

isHeadInChain :: NodeInfo -> AutolayoutState Bool
isHeadInChain = fmap isNothing . getPrevInChain

isLastInChain :: NodeInfo -> AutolayoutState Bool
isLastInChain = fmap isNothing . getNextInChain


removeCycles :: AutolayoutState ()
removeCycles = get >>= mapM_ removeCyclesForNode . Map.keys

removeCyclesForNode :: NodeLoc -> AutolayoutState ()
removeCyclesForNode nl = withJustM (lookupNode nl) $ \n -> when (n ^. dfsState == NotProccessed) $ do
    modify $ Map.update (\node -> Just $ node & dfsState .~ InProccess) nl
    let removeConnectionsWithNode :: NodeLoc -> [Connection] -> [Connection]
        removeConnectionsWithNode nl' = filter (\conn -> conn ^. srcNodeLoc /= nl'
                                                      && conn ^. dstNodeLoc /= nl')
        processDstNode :: NodeLoc -> AutolayoutState ()
        processDstNode dstNl = withJustM (lookupNode dstNl) $ \dstNode -> case dstNode ^. dfsState of
            NotProccessed -> removeCyclesForNode dstNl
            Proccessed -> return ()
            InProccess -> modify $
                Map.update (\node -> Just $ node & inConns  %~ removeConnectionsWithNode nl) dstNl .
                Map.update (\node -> Just $ node & outConns %~ removeConnectionsWithNode dstNl) nl
    forM_ (map (view dstNodeLoc) $ n ^. outConns) $ processDstNode
    modify $ Map.update (\node -> Just $ node & dfsState .~ Proccessed) nl

alignNeighbours :: NodeInfo -> AutolayoutState [NodeLoc]
alignNeighbours = fmap nub' . alignNeighboursX

alignNeighboursX :: NodeInfo -> AutolayoutState [NodeLoc]
alignNeighboursX n = do
    let prevX = n ^. actPos . x - gapBetweenNodes
        nextX = n ^. actPos . x + gapBetweenNodes
        proccessPred :: NodeInfo -> AutolayoutState (Maybe NodeLoc)
        proccessPred node = if node ^. actPos . x <= prevX then return Nothing else do
            let nl = node ^. nodeLoc
            modify $ Map.update (\n' -> Just $ n' & actPos . x .~ prevX) nl
            return $ Just nl
        proccessSucc :: NodeInfo -> AutolayoutState (Maybe NodeLoc)
        proccessSucc node = if node ^. actPos . x >= nextX then return Nothing else do
            let nl = node ^. nodeLoc
            modify $ Map.update (\n' -> Just $ n' & actPos . x .~ nextX) nl
            return $ Just nl
    preds <- lookupNodes . map (view srcNodeLoc) $ n ^. inConns
    succs <- lookupNodes . map (view dstNodeLoc) $ n ^. outConns
    predsToUpdate <- fmap catMaybes $ mapM proccessPred preds
    succsToUpdate <- fmap catMaybes $ mapM proccessSucc succs
    return $ predsToUpdate ++ succsToUpdate

-- alignNeighboursY :: NodeInfo -> AutolayoutState [NodeLoc]
-- alignNeighboursY n = do
--     let y' = n ^. actPos . y
--     preds <- lookupNodes . map (view srcNodeLoc) $ n ^. inConns
--     succs <- lookupNodes . map (view dstNodeLoc) $ n ^. outConns
--     fmap catMaybes . forM (preds ++ succs) $ \node ->
--         if node ^. actPos . y == y' || (not $ areInChain n node) then return Nothing else do
--             let nl = node ^. nodeLoc
--             modify $ Map.update (\n' -> Just $ n' & actPos . y .~ y') nl
--             return $ Just nl

alignChainsX :: NodeLoc -> AutolayoutState ()
alignChainsX nl = withJustM (lookupNode nl) $ \n -> do
    preds  <- lookupNodes . map (view srcNodeLoc) $ n ^. inConns
    succs  <- lookupNodes . map (view dstNodeLoc) $ n ^. outConns
    isLast <- isLastInChain n
    let alignToLeft  = not (null preds) && (null succs || (length succs == 1 && not isLast))
    if alignToLeft then do
        let maxPredX = maximum $ map (view $ actPos . x) preds
        when (maxPredX < n ^. actPos . x - gapBetweenNodes ) $ do
            modify $ Map.update (\n' -> Just $ n' & actPos . x .~ maxPredX + gapBetweenNodes)  (n ^. nodeLoc)
            forM_ succs $ alignChainsX . view nodeLoc
    else if not $ null succs then do
        let minSuccX = minimum $ map (view $ actPos . x) succs
        when (minSuccX > n ^. actPos . x + gapBetweenNodes ) $ do
            modify $ Map.update (\n' -> Just $ n' & actPos . x .~ minSuccX - gapBetweenNodes)  (n ^. nodeLoc)
            forM_ preds $ alignChainsX . view nodeLoc
    else return ()

sortOutConns :: [Connection] -> [Connection]
sortOutConns conns = do
    let sortFunction c1 c2 =
            if      c1 ^. dstPortId == Self then LT
            else if c2 ^. dstPortId == Self then GT
            else (c1 ^. srcPortId) `compare` (c2 ^. srcPortId)
    sortBy sortFunction conns
--
-- alignYForward :: NodeInfo -> AutolayoutState Double
-- alignYForward n = do
--     if n ^. dfsState /= NotProccessed then return (n ^. actPos . y + gapBetweenNodes) else do
--         modify $ Map.update (\n' -> Just $ n' & dfsState .~ Proccessed) (n ^. nodeLoc)
--         let sortedNls = map (view dstNodeLoc) . sortOutConns $ n ^. outConns
--             proccessNode maxY nl = do
--                 mayNode <- lookupNode nl
--                 case mayNode of
--                     Nothing   -> return maxY
--                     Just node -> if node ^. dfsState /= NotProccessed then return maxY else do
--                         modify $ Map.update (\n' -> Just $ n' & actPos . y .~ maxY) nl
--                         alignYForward $ node & actPos . y .~ maxY
--         maxY <- foldlM proccessNode (n ^. actPos . y) sortedNls
--         return $ max maxY (n ^. actPos . y + gapBetweenNodes)
--
sortInConns :: [Connection] -> [Connection]
sortInConns conns = do
    let sortFunction c1 c2 = compare (c1 ^. dstPortId) (c2 ^. dstPortId)
    sortBy sortFunction conns
--
-- alignYBackward :: NodeInfo -> AutolayoutState Double
-- alignYBackward n = do
--     if n ^. dfsState /= NotProccessed then return (n ^. actPos . y + gapBetweenNodes) else do
--         modify $ Map.update (\n' -> Just $ n' & dfsState .~ Proccessed) (n ^. nodeLoc)
--         let sortedNls = map (view srcNodeLoc) . sortInConns $ n ^. inConns
--             proccessNode maxY nl = do
--                 mayNode <- lookupNode nl
--                 case mayNode of
--                     Nothing   -> return maxY
--                     Just node -> if node ^. dfsState /= NotProccessed then return maxY else do
--                         modify $ Map.update (\n' -> Just $ n' & actPos . y .~ maxY) nl
--                         alignYBackward $ node & actPos . y .~ maxY
--         maxY <- foldlM proccessNode (n ^. actPos . y) sortedNls
--         return $ max maxY (n ^. actPos . y + gapBetweenNodes)

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing  Nothing  = Nothing
maxMaybe (Just a) Nothing  = Just a
maxMaybe Nothing  (Just b) = Just b
maxMaybe (Just a) (Just b) = Just $ max a b

findYPositionForward :: NodeInfo -> Subgraph -> AutolayoutState (Maybe Double)
findYPositionForward n s = do
    succs <- lookupNodes . map (view dstNodeLoc) . sortOutConns $ n ^. outConns
    let mayNYPos = (+gapBetweenNodes) . snd <$> (Map.lookup (n ^. actPos . x) $ s ^. shape)
    if null succs || (not . areOnTheSameLevel n $ head succs)
        then return mayNYPos
    else if isJust (view subgraphId $ head succs)
        then return . Just . (view $ actPos . y) $ head succs
        else maxMaybe mayNYPos <$> findYPositionForward (head succs) s

findYPositionBackward :: NodeInfo -> Subgraph -> AutolayoutState (Maybe Double)
findYPositionBackward n s = do
    preds <- lookupNodes . map (view srcNodeLoc) . sortInConns $ n ^. inConns
    let mayNYPos = (+gapBetweenNodes) . snd <$> (Map.lookup (n ^. actPos . x) $ s ^. shape)
    if null preds || (not . areOnTheSameLevel n $ head preds)
        then return mayNYPos
    else if isJust (view subgraphId $ head preds)
        then return . Just . (view $ actPos . y) $ head preds
        else maxMaybe mayNYPos <$> findYPositionBackward (head preds) s

findYPosition :: NodeInfo -> Subgraph -> AutolayoutState (Maybe Double)
findYPosition n s = maxMaybe <$> findYPositionBackward n s <*> findYPositionForward n s

findHigherNodeForward :: Subgraph -> NodeInfo -> AutolayoutState (Maybe NodeInfo)
findHigherNodeForward s n = do
    succs <- lookupNodes . map (view dstNodeLoc) . sortOutConns $ n ^. outConns
    if null succs || (isJust . view subgraphId $ head succs)
        then return Nothing
    else if areOnTheSameLevel n $ head succs
        then findHigherNodeForward s $ head succs
        else return . Just $ head succs

findHigherNodeBackward :: Subgraph -> NodeInfo -> AutolayoutState (Maybe NodeInfo)
findHigherNodeBackward s n = do
    preds <- lookupNodes . map (view srcNodeLoc) . sortInConns $ n ^. inConns
    if null preds || (isJust . view subgraphId $ head preds)
        then return Nothing
    else if areOnTheSameLevel n $ head preds
        then findHigherNodeBackward s $ head preds
        else return . Just $ head preds

findHigherNode :: Subgraph -> NodeInfo -> AutolayoutState (Maybe NodeInfo)
findHigherNode s n = do
    mayNode <- findHigherNodeBackward s n
    case mayNode of
        Just node -> return $ Just node
        Nothing   -> findHigherNodeForward s n

-- add consistency to subgraph position map
addToSubgraph :: Subgraph -> NodeInfo -> AutolayoutState Subgraph
addToSubgraph s n = if isJust $ n ^. subgraphId then return s else findHigherNode s n >>= \mayN ->
    if isJust mayN then addToSubgraph s $ fromJust mayN else do
        y' <- maybe (n ^. actPos . y) id <$> findYPosition n s
        modify $ Map.update (\n' -> Just $ n' & actPos . y .~ y'
                                              & subgraphId ?~ s ^. sid) (n ^. nodeLoc)
        let shapeAtX = maybe (y', y') (\(minY, maxY) -> (min minY y', max maxY y')) $ Map.lookup (n ^. actPos . x) $ s ^. shape
            updatedS :: Subgraph
            updatedS = s & nodeLocs %~ Set.insert (n ^. nodeLoc)
                         & shape    %~ Map.insert (n ^. actPos . x) shapeAtX
            proccessN :: Subgraph -> NodeLoc -> AutolayoutState Subgraph
            proccessN s' nl = lookupNode nl >>= maybe (return s') (addToSubgraph s')
        foldlM proccessN updatedS $ (map (view srcNodeLoc) . sortInConns  $ n ^. inConns)
                                 ++ (map (view dstNodeLoc) . sortOutConns $ n ^. outConns)

alignSubgraph :: Position -> Subgraph -> AutolayoutState Position
alignSubgraph pos s = case getSubgraphMinimumRectangle s of
    Nothing                     -> return pos
    Just (leftTop, rightBottom) -> do
        moveSubgraph (pos ^. vector - leftTop ^. vector) s
        return $ pos & y %~ (+ (rightBottom ^. y - leftTop ^. y + gapBetweenNodes))

moveSubgraph :: Vector2 Double -> Subgraph -> AutolayoutState ()
moveSubgraph shift s = mapM_ moveNode $ s ^. nodeLocs where
    moveNode nl = modify $ Map.update (\n -> Just $ n & actPos %~ move shift) nl

getSubgraphMinimumRectangle :: Subgraph -> Maybe (Position, Position)
getSubgraphMinimumRectangle s = if Map.null $ s ^. shape
    then Nothing
    else Just ( fromDoubles (minimum . Map.keys $ s ^. shape) (minimum . map fst . Map.elems $ s ^. shape)
              , fromDoubles (maximum . Map.keys $ s ^. shape) (maximum . map snd . Map.elems $ s ^. shape) )

alignNodesY :: Position -> AutolayoutState ()
alignNodesY pos = do
    nls <- Map.keys <$> get
    let makeSubgraph :: NodeInfo -> AutolayoutState Subgraph
        makeSubgraph n = addToSubgraph (Subgraph (n ^. nodeLoc) def def) n
    subgraphs <- fmap catMaybes . forM nls $ \nl -> lookupNode nl >>= \mayNode ->
        if isNothing mayNode || (isJust . join $ view subgraphId <$> mayNode)
            then return Nothing
            else forM mayNode makeSubgraph
    void $ foldlM alignSubgraph pos subgraphs
