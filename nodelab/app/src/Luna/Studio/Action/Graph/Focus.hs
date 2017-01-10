module Luna.Studio.Action.Graph.Focus
    ( focusNode
    , focusSelectedNode
    ) where

import           Data.Ord                        (comparing)
import           Luna.Studio.Action.Command      (Command)
import           Luna.Studio.Action.Graph.Lookup (allNodes)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node    (Node)
import qualified Luna.Studio.React.Model.Node    as Node
import qualified Luna.Studio.React.Model.Node    as Model
import           Luna.Studio.React.Store         (Ref, ref, widget)
import qualified Luna.Studio.React.Store         as Store
import           Luna.Studio.State.Global        (State)


nats :: [Integer]
nats = [1..]

focusNode :: Ref Node -> Command State ()
focusNode nodeRef = do
    node <- Store.get' nodeRef
    nodes <- mapM Store.get' =<< allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        equalFst a b = a ^. widget == b ^. widget
        newOrderNodes = node : deleteBy equalFst node sortedNodes
        newOrderRefs  = view ref <$> newOrderNodes
    forM_ (zip newOrderRefs nats) $ \(nRef, idx) -> do
        let newZPos = negate $ (fromIntegral idx) / 100.0
        Store.modify_ (Node.zPos .~ newZPos) nRef

focusSelectedNode :: Command State ()
focusSelectedNode = do
    return () --TODO[react]
    -- widgets <- selectedNodes
    -- inRegistry $ UIRegistry.focusedWidget .= (view ref <$> widgets ^? ix 0)
