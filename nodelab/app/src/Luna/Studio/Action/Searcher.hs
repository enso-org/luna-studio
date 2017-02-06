{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Action.Searcher where

import qualified Data.Map                           as Map

import           Data.Position                      (Position)
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as NodeAPI
import qualified Empire.API.Data.Port               as Port
import qualified Empire.API.Data.TypeRep            as TypeRep
import qualified Empire.API.Data.ValueType          as ValueType
import qualified JS.GoogleAnalytics                 as GA
import           Luna.Studio.Action.Camera          (translateToWorkspace)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Luna.Studio.Action.Node.Register   (registerNode)
import qualified Luna.Studio.Action.Node.Update     as Node
import qualified Luna.Studio.Batch.Workspace        as Workspace
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.App        as App
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.Searcher   as Searcher
import qualified Luna.Studio.React.View.App         as App
import qualified Luna.Studio.React.View.Searcher    as Searcher
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), Searcher (Searcher), searcherAction)
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph
import           Text.ScopeSearcher.Item            (Item (..), Items, _Group)
import qualified Text.ScopeSearcher.QueryResult     as Result
import qualified Text.ScopeSearcher.Scope           as Scope


instance Action (Command State) Searcher where
    begin    = beginActionWithKey    searcherAction
    continue = continueActionWithKey searcherAction
    update   = updateActionWithKey   searcherAction
    end      = close


searcherData :: Command State Items
searcherData = use $ Global.workspace . Workspace.nodeSearcherData

open :: Command State ()
open =
    openWith def =<< use Global.mousePos
    -- liftIO $ UI.initNodeSearcher "" Nothing (nsPos + offset) False

openWith :: Maybe NodeId -> Position -> Command State ()
openWith nodeId pos = do
    begin Searcher
    GA.sendEvent GA.NodeSearcher
    Global.modifyApp $ App.searcher ?= Searcher.Searcher pos 0 def def nodeId
    Global.renderIfNeeded
    liftIO Searcher.focus

close :: Searcher -> Command State ()
close _ = do
    Global.modifyApp $ App.searcher .= Nothing
    removeActionFromState searcherAction
    liftIO App.focus

moveDown :: Searcher -> Command State ()
moveDown _ = Global.modifySearcher $ do
    items <- length <$> use Searcher.results
    unless (items == 0) $
        Searcher.selected %= \p -> (p + 1) `mod` items

moveUp :: Searcher -> Command State ()
moveUp _ = Global.modifySearcher $ do
    items <- length <$> use Searcher.results
    unless (items == 0) $
        Searcher.selected %= \p -> (p - 1) `mod` items

accept :: Searcher -> Command State ()
accept action = do
    maySearcher <- Global.getSearcher
    withJust maySearcher $ \searcher -> do
        pos <- translateToWorkspace (searcher ^. Searcher.position)
        let selected  = searcher ^. Searcher.selected
            mayNodeId = searcher ^. Searcher.nodeId
            -- pos       = searcher ^. Searcher.position
            mayResult = listToMaybe $ drop selected $ searcher ^. Searcher.results
            expression = case mayResult of
                Just result -> result ^. Result.name
                Nothing -> searcher ^. Searcher.input
        case mayNodeId of
            Nothing -> registerNode pos expression
            Just nodeId-> Node.updateExpression nodeId expression
    close action

openEdit :: Text -> NodeId -> Position -> Command State ()
openEdit expr nodeId pos = do
    openWith (Just nodeId) pos
    continue $ querySearch expr

-- position :: Command State (Position, Position)
-- position = do
--     mousePos <- use Global.mousePos
--     mousePos' <- zoom Global.camera $ Camera.screenToWorkspaceM mousePos
--     -- factor <- use $ Global.camera . Camera.camera . Camera.factor
--     selected <- selectedNodes
--     nsPos <- zoom Global.camera $ Camera.workspaceToScreen $ case selected of
--             [wref] -> (wref ^. widget . Node.position) + Vector2 230.0 0
--             _      -> mousePos'
--     nsPos' <- zoom Global.camera $ Camera.screenToWorkspaceM nsPos
--     return (nsPos', nsPos)
--
-- ensureNSVisible :: Command State (Position, Position)
-- ensureNSVisible = do
--     (workspacePos, screenPos) <- position
--     screenSize <- use $ Global.camera . Camera.camera . Camera.screenSize
--     x' <- if screenPos ^. x > (screenSize ^. x - 250)
--         then do
--             Global.camera . Camera.camera . Camera.pan . x .= workspacePos ^. x
--             zoom Global.camera syncCamera
--             return . floor $ fromIntegral (screenSize ^. x) / (2.0 :: Double)
--         else return $ screenPos ^. x
--     y' <- if screenPos ^. y > (screenSize ^. y - 250)
--         then do
--             Global.camera . Camera.camera . Camera.pan . y .= workspacePos ^. y
--             zoom Global.camera syncCamera
--             return . floor $ fromIntegral (screenSize ^. y) / (2.0 :: Double)
--         else return $ screenPos ^. y
--
--     return (workspacePos, Vector2 x' y')

globalFunctions :: Items -> Items
globalFunctions = Map.filter (== Element)

scopedData :: Command State Items
scopedData = do
    completeData <- searcherData
    selected   <- selectedNodes
    mscope <- case selected of
            []     -> return Nothing
            [node]   -> do
                let nodeId = node ^. Node.nodeId
                mvt <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . NodeAPI.ports . ix (Port.OutPortId Port.All) . Port.valueType
                return $ case mvt of
                    Nothing -> Nothing
                    Just vt -> case vt of
                        ValueType.TypeIdent (TypeRep.TCons ti _) -> Just $ convert ti
                        _ -> Nothing
            (_:_) -> return Nothing
    case mscope of
        Nothing -> return completeData
        Just tn -> do
            let gf = globalFunctions completeData
                items = completeData
                mayScope = items ^? ix tn . _Group
                scope = fromMaybe mempty mayScope
                scopefuns = globalFunctions scope
                overallScope = Map.union scopefuns gf
            return overallScope


querySearch :: Text -> Searcher -> Command State ()
querySearch query _ = do
    sd <- scopedData
    let items = Scope.searchInScope sd query
    Global.modifySearcher $ do
        Searcher.input .= query
        s <- use Searcher.selected
        when (s >= length items) $
            Searcher.selected .= length items - 1
        Searcher.results .= items

-- queryTree :: Text -> Command State ()
-- queryTree query = do
--     sd <- scopedData
--     let items = Scope.moduleItems sd query
--     liftIO $ UI.displayTreeResults UI.NodeSearcher items
--
-- openCommand :: Command State ()
-- openCommand = do
--     GA.sendEvent GA.CommandSearcher
--     mousePos <- use Global.mousePos
--     liftIO $ UI.initNodeSearcher "" Nothing mousePos True
