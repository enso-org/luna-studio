{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Searcher where


import qualified Data.Map                          as Map
import qualified Data.Text.Lazy                    as Text
import           Utils.PreludePlus
import           Utils.Vector
import qualified Text.ScopeSearcher.QueryResult as Result

import qualified JS.NodeSearcher                   as UI

import qualified Batch.Workspace                   as Workspace
import           React.Store                       (widget)
import qualified React.Store                       as Store
import qualified React.Store.Node                  as Node
import qualified React.Store.Searcher              as Searcher
import qualified React.View.App                    as App
import qualified React.View.Searcher               as Searcher

import           Reactive.Commands.Camera          (syncCamera)
import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import Reactive.Commands.Node.Register (registerNode)

import           Empire.API.Data.Node              (NodeId)
import qualified Empire.API.Data.Node              as NodeAPI
import qualified Empire.API.Data.Port              as Port
import qualified Empire.API.Data.TypeRep           as TypeRep
import qualified Empire.API.Data.ValueType         as ValueType

import qualified JS.GoogleAnalytics                as GA
import           Text.ScopeSearcher.Item           (Item (..), Items, _Group)
import qualified Text.ScopeSearcher.Scope          as Scope



searcherData :: Command State Items
searcherData = use $ Global.workspace . Workspace.nodeSearcherData

open :: Command State ()
open = do
    GA.sendEvent GA.NodeSearcher
    -- factor <- use $ Global.camera . Camera.camera . Camera.factor
    -- let offset = Vector2 0 (floor $ -40.0 * factor)
    -- (nsPos', nsPos) <- ensureNSVisible
    -- Global.uiElements . UIElements.nsPos .= nsPos'
    Global.withSearcher $ Store.modifyM_ $ do
        Searcher.results .= def
        Searcher.visible .= True
        Searcher.selected .= 0
    liftIO Searcher.focus
    -- performIO $ UI.initNodeSearcher "" Nothing (nsPos + offset) False

close :: Command State ()
close = do
    Global.withSearcher $ Store.modify_ $ Searcher.visible .~ False
    liftIO App.focus

moveDown :: Command State ()
moveDown = Global.withSearcher $ Store.modifyM_ $ do
    items <- length <$> use Searcher.results
    Searcher.selected %= \p -> (p + 1) `mod` items

moveUp :: Command State ()
moveUp = Global.withSearcher $ Store.modifyM_ $ do
    items <- length <$> use Searcher.results
    Searcher.selected %= \p -> (p - 1) `mod` items

accept :: Command State ()
accept = do
    searcher <- Global.withSearcher $ Store.get
    let selected = searcher ^. Searcher.selected
        mayResult = listToMaybe $ drop selected $ searcher ^. Searcher.results
    registerNode $ case mayResult of
        Just result -> result ^. Result.name
        Nothing -> searcher ^. Searcher.input
    close

openEdit :: Text -> NodeId -> Vector2 Int -> Command State ()
openEdit expr nodeId pos = do
    performIO $ UI.initNodeSearcher expr (Just nodeId) pos False

position :: Command State (Vector2 Double, Vector2 Int)
position = do
    mousePos <- use Global.mousePos
    mousePos' <- zoom Global.camera $ Camera.screenToWorkspaceM mousePos
    -- factor <- use $ Global.camera . Camera.camera . Camera.factor
    selected <- selectedNodes
    nsPos <- zoom Global.camera $ Camera.workspaceToScreen $ case selected of
            [wref] -> (wref ^. widget . Node.position) + Vector2 230.0 0
            _      -> mousePos'
    nsPos' <- zoom Global.camera $ Camera.screenToWorkspaceM nsPos
    return (nsPos', nsPos)

ensureNSVisible :: Command State (Vector2 Double, Vector2 Int)
ensureNSVisible = do
    (workspacePos, screenPos) <- position
    screenSize <- use $ Global.camera . Camera.camera . Camera.screenSize
    x' <- if screenPos ^. x > (screenSize ^. x - 250)
        then do
            Global.camera . Camera.camera . Camera.pan . x .= workspacePos ^. x
            zoom Global.camera syncCamera
            return . floor $ fromIntegral (screenSize ^. x) / (2.0 :: Double)
        else return $ screenPos ^. x
    y' <- if screenPos ^. y > (screenSize ^. y - 250)
        then do
            Global.camera . Camera.camera . Camera.pan . y .= workspacePos ^. y
            zoom Global.camera syncCamera
            return . floor $ fromIntegral (screenSize ^. y) / (2.0 :: Double)
        else return $ screenPos ^. y

    return (workspacePos, Vector2 x' y')

globalFunctions :: Items -> Items
globalFunctions = Map.filter (== Element)

scopedData :: Command State Items
scopedData = do
    completeData <- searcherData
    selected   <- selectedNodes
    mscope <- case selected of
            []     -> return Nothing
            [wf]   -> do
                let nodeId = wf ^. widget . Node.nodeId
                mvt <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . NodeAPI.ports . ix (Port.OutPortId Port.All) . Port.valueType
                return $ case mvt of
                    Nothing -> Nothing
                    Just vt -> case vt of
                        ValueType.TypeIdent (TypeRep.TCons ti _) -> Just $ Text.pack ti
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


querySearch :: Text -> Command State ()
querySearch query = do
    sd <- scopedData
    let items = Scope.searchInScope sd query
    Global.withSearcher $ Store.modifyM_ $ do
        Searcher.input .= query
        s <- use Searcher.selected
        when (s >= length items) $
            Searcher.selected .= length items - 1
        Searcher.results .= items

queryTree :: Text -> Command State ()
queryTree query = do
    sd <- scopedData
    let items = Scope.moduleItems sd query
    performIO $ UI.displayTreeResults UI.NodeSearcher items

openCommand :: Command State ()
openCommand = do
    GA.sendEvent GA.CommandSearcher
    mousePos <- use Global.mousePos
    performIO $ UI.initNodeSearcher "" Nothing mousePos True
