{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Plugins.Core.Action.MultiSelection where

import           Control.Monad.State               (modify)
import qualified Data.Set                          as Set
import           JS.MultiSelection                 (displaySelectionBox, hideSelectionBox)
import           Utils.PreludePlus
import           Utils.Vector                      (Vector2 (..), x, y)

import           Object.Widget                     (objectId, widget)
import qualified Object.Widget.Node                as NodeModel

import           Event.Event                       (Event (Keyboard, Mouse), JSState)
import           Event.Keyboard                    (KeyMods (..))
import qualified Event.Keyboard                    as Keyboard
import qualified Event.Mouse                       as Mouse

import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Global             (State, inRegistry)
import qualified Reactive.State.Global             as Global
import           Reactive.State.MultiSelection     (DragHistory (..))
import qualified Reactive.State.MultiSelection     as MultiSelection
import qualified Reactive.State.UIRegistry         as UIRegistry

import           Reactive.Commands.Batch           (cancelCollaborativeTouch, collaborativeTouch)
import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Graph.Selection (focusSelectedNode,
                                                    modifySelectionHistory,
                                                    selectAll,
                                                    selectedNodes,
                                                    unselectAll,
                                                    unselectAllAndDropSelectionHistory)
import qualified Reactive.Commands.UIRegistry      as UICmd

import           UI.Raycaster                      (getObjectsInRect)


--TODO[react] implement
toAction :: Event -> Maybe (Command State ())
-- toAction (Mouse _       (Mouse.Event Mouse.Pressed  pos Mouse.LeftButton (KeyMods False False False False) Nothing)) = Just $ startDrag pos
-- toAction (Mouse jsstate (Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove jsstate pos
-- toAction (Mouse _       (Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag
--
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Press 'A'   _)) = Just trySelectAll
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Down  '\27' _)) = Just tryUnselectAll
toAction _ = Nothing

-- trySelectAll :: Command State ()
-- trySelectAll = do
--     focusedWidget <- inRegistry $ use UIRegistry.focusedWidget
--     when (isNothing focusedWidget) selectAll
--
-- tryUnselectAll :: Command State ()
-- tryUnselectAll = do
--     focusedWidget <- inRegistry $ use UIRegistry.focusedWidget
--     when (isNothing focusedWidget) unselectAllAndDropSelectionHistory
--
-- startDrag :: Vector2 Int -> Command State ()
-- startDrag coord = do
--     Global.multiSelection . MultiSelection.history ?= DragHistory coord coord
--     unselectAll
--
-- handleMove :: JSState -> Vector2 Int -> Command State ()
-- handleMove jsstate coord = do
--     dragHistory <- use $ Global.multiSelection . MultiSelection.history
--     case dragHistory of
--         Nothing                          -> return ()
--         Just (DragHistory start _current) -> do
--             Global.multiSelection . MultiSelection.history ?= DragHistory start coord
--             updateSelection jsstate start coord
--             drawSelectionBox start coord
--
-- updateSelection :: JSState -> Vector2 Int -> Vector2 Int -> Command State ()
-- updateSelection jsstate start end = do
--     let leftTop     = Vector2 (min (start ^. x) (end ^. x)) (min (start ^. y) (end ^. y))
--         rightBottom = Vector2 (max (start ^. x) (end ^. x)) (max (start ^. y) (end ^. y))
--         ids         = getObjectsInRect jsstate leftTop (rightBottom - leftTop)
--     oldSelected <- selectedNodes
--     newSelectedFiles <-  inRegistry $ mapM widgetIdToNodeWidget ids
--     let oldSet     = Set.fromList $ view objectId <$> oldSelected
--         newSet     = Set.fromList $ view objectId <$> catMaybes newSelectedFiles
--         toSelect   = Set.difference newSet oldSet
--         toUnselect = Set.difference oldSet newSet
--     inRegistry $ do
--         forM_ toSelect   $ flip UICmd.update_ $ NodeModel.isSelected .~ True
--         forM_ toUnselect $ flip UICmd.update_ $ NodeModel.isSelected .~ False
--     do
--         let oldSet     = Set.fromList $ view (widget . NodeModel.nodeId) <$> oldSelected
--             newSet     = Set.fromList $ view (widget . NodeModel.nodeId) <$> catMaybes newSelectedFiles
--             toSelect   = Set.difference newSet oldSet
--             toUnselect = Set.difference oldSet newSet
--         collaborativeTouch $ Set.toList toSelect
--         cancelCollaborativeTouch $ Set.toList toUnselect
--
-- drawSelectionBox :: Vector2 Int -> Vector2 Int -> Command State ()
-- drawSelectionBox start end = do
--     camera <- use $ Global.camera . Camera.camera
--     let startSelectionBox = Camera.screenToWorkspace camera start
--         endSelectionBox   = Camera.screenToWorkspace camera end
--     performIO $ displaySelectionBox startSelectionBox endSelectionBox
--
-- stopDrag :: Command State ()
-- stopDrag = do
--     wasSelecting <- uses (Global.multiSelection . MultiSelection.history) isJust
--     when wasSelecting $ do
--         Global.multiSelection . MultiSelection.history .= Nothing
--         performIO hideSelectionBox
--         focusSelectedNode
--         selectedWidgets <- selectedNodes
--         let selectedNodesIds = map (^. widget . NodeModel.nodeId) selectedWidgets
--         modifySelectionHistory selectedNodesIds
