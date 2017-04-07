{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.Autolayout where

import           Control.Monad.State.Lazy                    (execState, get, modify)
import qualified Control.Monad.State.Lazy                    as S
import           Data.Map.Lazy                               (Map)
import qualified Data.Map.Lazy                               as Map
import           Data.Position                               (Position, minimumRectangle, x, y)
import           Luna.Studio.Action.Basic                    (moveNodes)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getConnectionsFromNode, getConnectionsToNode, getExpressionNodes,
                                                              getSelectedNodes)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection          (Connection, dstNodeLoc, dstPortId, srcNodeLoc)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as Node
import           Luna.Studio.React.Model.Port                (PortId (InPortId), isSelf)
import           Luna.Studio.State.Global                    (State)


data DFSState = NotProccessed | InProccess | Proccessed deriving Eq
data NodeInfo = NodeInfo { _nodeLoc  :: NodeLoc
                         , _name     :: Text
                         , _actPos   :: Position
                         , _dfsState :: DFSState
                         , _inConns  :: [Connection]
                         , _outConns :: [Connection]
                         }
makeLenses ''NodeInfo

type NodesInfoMap      = Map NodeLoc NodeInfo
type AutolayoutState a = S.State NodesInfoMap a


gapBetweenNodes :: Double
gapBetweenNodes = 150

autolayoutAllNodes :: Command State ()
autolayoutAllNodes = getExpressionNodes >>= autolayoutNodes

autolayoutSelectedNodes :: Command State ()
autolayoutSelectedNodes = getSelectedNodes >>= autolayoutNodes

autolayoutNodes :: [ExpressionNode] -> Command State ()
autolayoutNodes nodes = do
    let mayMinRect = minimumRectangle $ map (view Node.position) nodes
    withJust mayMinRect $ \(leftTop, rightBottom) -> do
        let x' = ((leftTop ^. x) + (rightBottom ^. x)) / 2
        nodesMap <- fmap Map.fromList $ forM nodes $ \n -> do
            let nl = n ^. Node.nodeLoc
            inConns'  <- getConnectionsToNode   nl
            outConns' <- getConnectionsFromNode nl
            return $ (nl, NodeInfo nl
                                   (n ^. Node.name)
                                   (n ^. Node.position & x .~ x')
                                   NotProccessed
                                   inConns'
                                   outConns' )
        moveNodes . Map.toList . fmap (view actPos) $ execState findPositions nodesMap


findPositions :: AutolayoutState ()
findPositions = do
    removeCycles
    nls <- Map.keys <$> get
    mapM_ findPositionRecursive nls
    mapM_ alignToLeft nls

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
alignNeighbours n = fmap nub' $ (++) <$> alignNeighboursX n <*> alignNeighboursY n

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

alignNeighboursY :: NodeInfo -> AutolayoutState [NodeLoc]
alignNeighboursY n = do
    let y' = n ^. actPos . y
    preds <- lookupNodes . map (view srcNodeLoc) $ n ^. inConns
    succs <- lookupNodes . map (view dstNodeLoc) $ n ^. outConns
    fmap catMaybes . forM (preds ++ succs) $ \node ->
        if node ^. actPos . y == y' || (not $ areInChain n node) then return Nothing else do
            let nl = node ^. nodeLoc
            modify $ Map.update (\n' -> Just $ n' & actPos . y .~ y') nl
            return $ Just nl

alignToLeft :: NodeLoc -> AutolayoutState ()
alignToLeft nl = withJustM (lookupNode nl) $ \n -> do
    preds  <- lookupNodes . map (view srcNodeLoc) $ n ^. inConns
    succs  <- lookupNodes . map (view dstNodeLoc) $ n ^. outConns
    isLast <- isLastInChain n
    let maxPredX = maximum $ map (view $ actPos . x) preds
        needMove = not (null preds)
                && length succs == 1
                && not isLast
                && maxPredX < n ^. actPos . x - gapBetweenNodes
    when needMove $ do
        modify $ Map.update (\n' -> Just $ n' & actPos . x .~ maxPredX + gapBetweenNodes)  (n ^. nodeLoc)
        forM_ succs $ alignToLeft . view nodeLoc
