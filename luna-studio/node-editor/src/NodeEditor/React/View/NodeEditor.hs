{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.NodeEditor where


import           Common.Prelude                             hiding (transform)
import qualified Data.HashMap.Strict                        as HashMap
import qualified Data.Matrix                                as Matrix
import           Data.Maybe                                 (mapMaybe)
import           JS.Scene                                   (sceneId)
import qualified LunaStudio.Data.MonadPath                  as MonadPath
import           LunaStudio.Data.NodeLoc                    (NodePath)
import           LunaStudio.Data.PortRef                    (InPortRef (InPortRef))
import qualified NodeEditor.Data.CameraTransformation       as CameraTransformation
import           NodeEditor.Data.Matrix                     (showCameraMatrix, showCameraScale, showCameraTranslate)
import           NodeEditor.Event.Event                     (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import qualified NodeEditor.Event.UI                        as UI
import qualified NodeEditor.React.Event.NodeEditor          as NE
import           NodeEditor.React.Model.App                 (App)
import qualified NodeEditor.React.Model.Connection          as Connection
import qualified NodeEditor.React.Model.Node                as Node
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import           NodeEditor.React.Model.NodeEditor          (NodeEditor)
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import           NodeEditor.React.Model.Port                (InPortIndex (Self))
import qualified NodeEditor.React.Model.Searcher            as Searcher
import           NodeEditor.React.Store                     (Ref, dispatch, dispatch')
import           NodeEditor.React.View.Connection           (connection_, halfConnection_)
import           NodeEditor.React.View.ConnectionPen        (connectionPen_)
import           NodeEditor.React.View.ExpressionNode       (filterOutSearcherIfNotRelated, nodeDynamicStyles_, node_)
import           NodeEditor.React.View.Monad                (monads_)
import           NodeEditor.React.View.Plane                (planeCanvas_, planeConnections_, planeMonads_, planeNodes_, svgPlanes_)
import           NodeEditor.React.View.SelectionBox         (selectionBox_)
import           NodeEditor.React.View.Sidebar              (sidebar_)
import qualified NodeEditor.React.View.Style                as Style
import           NodeEditor.React.View.Visualization        (pinnedVisualization_)
import           Numeric                                    (showFFloat)
import           React.Flux                                 hiding (transform)
import qualified React.Flux                                 as React


name :: JSString
name = "node-editor"

show1 :: Double -> String
show1 a = showFFloat (Just 1) a "" -- limit Double to two decimal numbers TODO: remove before the release

show4 :: Double -> String
show4 a = showFFloat (Just 4) a "" -- limit Double to two decimal numbers TODO: remove before the release

applySearcherHints :: NodeEditor -> NodeEditor
applySearcherHints ne = maybe ne replaceNode $ ne ^. NodeEditor.searcher where
    connect srcPortRef dstPortRef ne' = ne' & NodeEditor.connections . at dstPortRef ?~ Connection.Connection srcPortRef dstPortRef Connection.Normal
    tryConnect nl nn ne' = maybe ne' (\srcPortRef -> connect srcPortRef (InPortRef nl [Self]) ne') $ nn ^. Searcher.predPortRef
    toModel n nl pos = (convert (def :: NodePath, n)) & ExpressionNode.nodeLoc  .~ nl
                                                      & ExpressionNode.position .~ pos
    updateNode nl n ne' = maybe ne' (flip NodeEditor.updateExpressionNode ne . Searcher.applyExpressionHint n) $ NodeEditor.getExpressionNode nl ne'
    replaceNode s    = case (s ^. Searcher.mode, s ^. Searcher.selectedNode) of
        (Searcher.Node nl (Searcher.NodeModeInfo _ Nothing   _) _, Just n) -> updateNode nl n ne
        (Searcher.Node nl (Searcher.NodeModeInfo _ (Just nn) _) _, Just n) -> tryConnect nl nn $ NodeEditor.updateExpressionNode (toModel n nl (nn ^. Searcher.position)) ne
        (Searcher.Node nl (Searcher.NodeModeInfo _ (Just nn) _) _, _)      -> tryConnect nl nn $ NodeEditor.updateExpressionNode (ExpressionNode.mkExprNode nl (s ^. Searcher.inputText) (nn ^. Searcher.position)) ne
        _                                                                  -> ne

nodeEditor_ :: Ref App -> NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref ne = React.viewWithSKey nodeEditor name (ref, ne) mempty

nodeEditor :: ReactView (Ref App, NodeEditor)
nodeEditor = React.defineView name $ \(ref, ne') -> do
    let ne           = applySearcherHints ne'
        camera       = ne ^. NodeEditor.screenTransform . CameraTransformation.logicalToScreen
        nodes        = ne ^. NodeEditor.expressionNodes . to HashMap.elems
        input        = ne ^. NodeEditor.inputNode
        output       = ne ^. NodeEditor.outputNode
        lookupNode m = ( m ^. MonadPath.monadType
                       , m ^. MonadPath.path . to (mapMaybe $ flip HashMap.lookup $ ne ^. NodeEditor.expressionNodes))
        monads       = map lookupNode $ ne ^. NodeEditor.monads
        scale        = (Matrix.toList camera)!!0 :: Double
        maySearcher  = ne ^. NodeEditor.searcher
    if ne ^. NodeEditor.isGraphLoaded then
        div_
            [ "className"   $= Style.prefix "graph"
            , "id"          $= sceneId
            , "key"         $= "graph"
            , onMouseDown   $ \_ m   -> dispatch ref $ UI.NodeEditorEvent $ NE.MouseDown m
            , onDoubleClick $ \_ _   -> dispatch' ref $ Shortcut $ Shortcut.Event Shortcut.ExitGraph def
            , onWheel       $ \e m w -> preventDefault e : dispatch ref (UI.NodeEditorEvent $ NE.Wheel m w)
            , onScroll      $ \e     -> [preventDefault e]
            ] $ do

            style_
                [ "key" $= "style"
                ] $ do

                elemString $ ":root { font-size: " <> show scale <> "px }"
                elemString $ ":root { --scale: "   <> show scale <> " }"

                elemString $ ".luna-camera-scale { transform: "     <> showCameraScale     camera <> " }"
                elemString $ ".luna-camera-translate { transform: " <> showCameraTranslate camera <> " }"
                elemString $ ".luna-camera-transform { transform: " <> showCameraMatrix    camera <> " }"

                elemString $ ".luna-connection__line { stroke-width: "   <> show (1.2 + (1 / scale)) <> " }"
                elemString $ ".luna-connection__select { stroke-width: " <> show (10/scale)          <> " }"

                --collapsed nodes
                elemString $ ".luna-port-io-shape-mask { r: "  <> show (19.2 + (0.8 / scale)) <> "px }"
                elemString $ ".luna-port-io-select-mask { r: " <> show (19.2 + (0.8 / scale)) <> "px }"

                --expanded nodes
                elemString $ "circle.luna-port__shape { r: " <> show (3 + (1 / scale)) <> "px }"
                elemString $ ".luna-port--alias circle.luna-port__shape { r: " <> show (7 + (1 / scale)) <> "px }"

                forM_ (ne ^. NodeEditor.expressionNodesRecursive) $ nodeDynamicStyles_ camera

            svgPlanes_ $ do
                planeMonads_ $
                    monads_ monads
                planeConnections_ $ do
                    forM_     (ne ^. NodeEditor.posConnections ) $ connection_ ref
                    forKeyed_ (ne ^. NodeEditor.posHalfConnections) $ uncurry halfConnection_
                    forM_     (ne ^. NodeEditor.selectionBox   ) selectionBox_
                    forM_     (ne ^. NodeEditor.connectionPen  ) connectionPen_

            planeNodes_ $ do
                forM_  nodes                            $ \n -> node_ ref n (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maySearcher)
                forM_ (ne ^. NodeEditor.visualizations) $ pinnedVisualization_ ref ne


            forM_ input  $ \n -> sidebar_ ref (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maySearcher) n
            forM_ output $ sidebar_ ref Nothing

            planeCanvas_ mempty
    else
        div_ [ "className"   $= Style.prefix "graph"] $
            elemString "No file selected"
