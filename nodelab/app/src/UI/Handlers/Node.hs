{-# LANGUAGE OverloadedStrings #-}

module UI.Handlers.Node where

import           Luna.Studio.Prelude                         hiding (stripPrefix)

import           Control.Monad.Trans.State                   (get)
import qualified Data.HashMap.Strict                         as HashMap
import           Data.HMap.Lazy                              (HTMap, TypeKey (..))
import qualified Data.Text.Lazy                              as Text
import           Luna.Studio.Data.Vector

import           Object.Widget                               (CompositeWidget, KeyPressedHandler, ResizableWidget, UIHandlers, WidgetId,
                                                              createWidget, keyDown, mouseOut, mouseOver, updateWidget)

import           Luna.Studio.React.Model.Node                (Node)
import qualified Luna.Studio.React.Model.Node                as Node
import qualified Luna.Studio.React.Model.NodeEditor          as NodeEditor
import           Luna.Studio.React.Store                     (Ref, WRef)
import qualified Luna.Studio.React.Store                     as Store

import           Luna.Studio.Commands.Batch                  (cancelCollaborativeTouch, collaborativeTouch)
import           Luna.Studio.Commands.Command                (Command)
import           Luna.Studio.Commands.Graph.SelectionHistory (dropSelectionHistory, modifySelectionHistory)
import qualified Luna.Studio.Commands.UIRegistry             as UICmd
import qualified Luna.Studio.React.Model.Node                as Model
import           Luna.Studio.State.Global                    (inRegistry)
import qualified Luna.Studio.State.Global                    as Global
import           Luna.Studio.State.UIRegistry                (addHandler)
import qualified Luna.Studio.State.UIRegistry                as UIRegistry
import qualified Object.Widget.CodeEditor                    as CodeEditor
import qualified Object.Widget.Group                         as Group
import qualified Object.Widget.Label                         as Label
import qualified Object.Widget.TextBox                       as TextBox

import qualified Style.Node                                  as Style
import           UI.Generic                                  (whenChanged)
import           UI.Handlers.Generic                         (ValueChangedHandler (..))
import           UI.Widget.CodeEditor                        ()
import           UI.Widget.Group                             ()
import           UI.Widget.TextBox                           ()

import           Empire.API.Data.Node                        (NodeId)



-- TODO[react]: Does not make sense anymore
-- typeHandlers :: WidgetId -> HTMap
-- typeHandlers wid = addHandler (ValueChangedHandler $ typeValueChangedHandler wid)
--                 $ addHandler (UICmd.LostFocus $ inRegistry . flip UICmd.update_ (TextBox.isEditing .~ False))
--                 $ mempty where
--
-- typeValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
-- typeValueChangedHandler parent val _tbId = do
--     model <- inRegistry $ UICmd.update parent $ Model.tpe ?~ val
--     triggerChangeInputNodeTypeHandler parent model
--
-- codeEditorChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
-- codeEditorChangedHandler nodeWidgetId newCode _ = triggerCodeChangedHandler nodeWidgetId newCode
--
-- codeHandlers :: WidgetId -> HTMap
-- codeHandlers wid = addHandler (ValueChangedHandler $ codeEditorChangedHandler wid)
--                 $ mempty

newtype ChangeInputNodeTypeHandler = ChangeInputNodeTypeHandler (WidgetId -> NodeId -> Text -> Command Global.State ())
changeInputNodeTypeHandler = TypeKey :: TypeKey ChangeInputNodeTypeHandler

newtype CodeChangedHandler = CodeChangedHandler (NodeId -> Text -> Command Global.State ())
codeChangedHandler = TypeKey :: TypeKey CodeChangedHandler

triggerChangeInputNodeTypeHandler :: WidgetId -> Model.Node -> Command Global.State ()
triggerChangeInputNodeTypeHandler wid model = do
    withJust (model ^. Model.tpe) $ \tpe -> do
        maybeHandler <- inRegistry $ UICmd.handler wid changeInputNodeTypeHandler
        withJust maybeHandler $ \(ChangeInputNodeTypeHandler handler) -> handler wid (model ^. Model.nodeId) tpe

-- TODO[react]: Does not make sense anymore
-- triggerCodeChangedHandler :: WidgetId -> Text -> Command Global.State ()
-- triggerCodeChangedHandler wid newCode = do
--     nodeId       <- inRegistry $ UICmd.get wid Model.nodeId
--     maybeHandler <- inRegistry $ UICmd.handler wid codeChangedHandler
--     withJust maybeHandler $ \(CodeChangedHandler handler) -> handler nodeId newCode
--
-- showHidePortLabels :: Bool -> WidgetId -> Command UIRegistry.State ()
-- showHidePortLabels show wid = do
--     inLabels <- inLabelsGroupId wid
--     UICmd.update_ inLabels $ Group.visible .~ show
--     outLabels <- outLabelsGroupId wid
--     UICmd.update_ outLabels $ Group.visible .~ show
--
-- onMouseOver, onMouseOut :: WidgetId -> Command Global.State ()
-- onMouseOver wid = inRegistry $ do
--     UICmd.update_ wid $ Model.highlight .~ True
--     showHidePortLabels True wid
-- onMouseOut  wid = inRegistry $ do
--     UICmd.update_ wid $ Model.highlight .~ False
--     showHidePortLabels False wid
--
-- widgetHandlers :: UIHandlers Global.State
-- widgetHandlers = def & mouseOver .~ const onMouseOver
--                      & mouseOut  .~ const onMouseOut
--
-- displayCodeEditor :: WidgetId -> WidgetId -> Text -> Command UIRegistry.State WidgetId
-- displayCodeEditor nodeWidgetId nodeGroupId code = do
--     let widget = CodeEditor.create Style.codeEditorSize code
--     UICmd.register nodeGroupId widget $ codeHandlers nodeWidgetId

instance ResizableWidget Model.Node

-- TODO[react]: Does not make sense anymore
-- instance CompositeWidget Model.Node where
--     createWidget wid model = do
--         let grp    = Group.create & Group.size .~ Vector2 1 1
--         portGroup <- UICmd.register wid grp def
--
--         let group  = Group.create & Group.position .~ Style.controlsPosition
--         controlGroups <- UICmd.register wid group Style.controlsLayout
--
--         let inLabelsGroup  = Group.create & Group.position .~ Vector2 (-400) (-30)
--                                           & Group.visible .~ False
--         inLabelsGroupId <- UICmd.register wid inLabelsGroup Style.inLabelsLayout
--
--         let outLabelsGroup  = Group.create & Group.position .~ Vector2 40 (-30)
--                                            & Group.visible .~ False
--         outLabelsGroupId <- UICmd.register wid outLabelsGroup Style.inLabelsLayout
--
--         let grp    = Group.create & Group.style   .~ Style.expandedGroupStyle
--                                   & Group.visible .~ (model ^. Model.isExpanded)
--         expandedGroup <- UICmd.register controlGroups grp Style.expandedGroupLayout
--
--         nodeGroupId <- UICmd.register expandedGroup Group.create Style.expandedGroupLayout
--
--         codeEditorId <- mapM (displayCodeEditor wid nodeGroupId) $ model ^. Model.code
--
--         let grp    = Group.create
--         portControlsGroupId <- UICmd.register expandedGroup grp Style.expandedGroupLayout
--
--         void $ UIRegistry.updateWidgetM wid $ Model.elements %~ ( (Model.expandedGroup       .~ expandedGroup              )
--                                                                . (Model.nodeGroup           .~ nodeGroupId                )
--                                                                . (Model.portControls        .~ portControlsGroupId        )
--                                                                . (Model.inLabelsGroup       .~ inLabelsGroupId            )
--                                                                . (Model.outLabelsGroup      .~ outLabelsGroupId           )
--                                                                . (Model.codeEditor          .~ codeEditorId               )
--                                                                )
--
--     updateWidget wid old model = do
--         whenChanged old model Model.isExpanded $ do
--             let controlsId = model ^. Model.elements . Model.expandedGroup
--             UICmd.update_ controlsId $ Group.visible .~ (model ^. Model.isExpanded)
--
--         withJust (model ^. Model.code) $ \codeBody -> do
--             let nodeGroupId = model ^. Model.elements . Model.nodeGroup
--
--             when (isNothing $ model ^. Model.elements . Model.codeEditor) $ do
--                 codeEditorId <- displayCodeEditor wid nodeGroupId codeBody
--                 void $ UIRegistry.updateWidgetM wid $ Model.elements . Model.codeEditor .~ Just codeEditorId
--
--             withJust (model ^. Model.elements . Model.codeEditor) $ \codeEditorId ->
--                 UICmd.update_ codeEditorId $ CodeEditor.value .~ codeBody
--
--         when (isNothing $ model ^. Model.code) $
--             withJust (model ^. Model.elements . Model.codeEditor) $ \codeEditorId -> do
--                 UICmd.removeWidget codeEditorId
--                 void $ UIRegistry.updateWidgetM wid $ Model.elements . Model.codeEditor .~ Nothing


-- portControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
-- portControlsGroupId wid = UICmd.get wid $ Model.elements . Model.portControls
--
-- inLabelsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
-- inLabelsGroupId wid = UICmd.get wid $ Model.elements . Model.inLabelsGroup
--
-- outLabelsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
-- outLabelsGroupId wid = UICmd.get wid $ Model.elements . Model.outLabelsGroup

trimExpression :: Text -> Text
trimExpression expr
    | Text.length expr < 20 = expr
    | otherwise             = Text.take 20 expr <> "…"
