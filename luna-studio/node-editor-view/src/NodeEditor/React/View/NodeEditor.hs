{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.NodeEditor where

import           Common.Prelude                             hiding (transform)
import qualified Data.HashMap.Strict                        as HashMap
import           Data.Matrix                                (Matrix)
import           Data.Maybe                                 (mapMaybe)
import qualified Data.Set                                   as Set
import qualified LunaStudio.Data.CameraTransformation       as CameraTransformation
import           LunaStudio.Data.Matrix                     (CameraScale, CameraTranslate, showCameraMatrix, showCameraTranslate)
import qualified LunaStudio.Data.Matrix                     as Matrix
import qualified LunaStudio.Data.MonadPath                  as MonadPath
import           LunaStudio.Data.NodeLoc                    (NodePath)
import           LunaStudio.Data.PortRef                    (InPortRef (InPortRef))
import           NodeEditor.React.IsRef                     (IsRef)
import qualified NodeEditor.React.Model.Connection          as Connection
import qualified NodeEditor.React.Model.Node                as Node
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import           NodeEditor.React.Model.NodeEditor          (GraphStatus (..), NodeEditor)
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import           NodeEditor.React.Model.Port                (InPortIndex (Self))
import qualified NodeEditor.React.Model.Searcher            as Searcher
import           NodeEditor.React.Model.Visualization       (VisualizationMode (Focused, FullScreen, Preview), visPropNodeLoc,
                                                             visPropVisualization, visualizationMode)
import           NodeEditor.React.View.Connection           (connection_, halfConnection_)
import           NodeEditor.React.View.ConnectionPen        (connectionPen_)
import           NodeEditor.React.View.ExpressionNode       (filterOutEditedTextControlIfNotRelated, filterOutSearcherIfNotRelated,
                                                             nodeDynamicStyles_, node_)
import           NodeEditor.React.View.Monad                (monads_)
import           NodeEditor.React.View.Plane                (planeCanvas_, planeConnections_, planeMonads_, planeNewConnection_,
                                                             planeNodes_)
import           NodeEditor.React.View.SelectionBox         (selectionBox_)
import           NodeEditor.React.View.Sidebar              (sidebar_)
import qualified NodeEditor.React.View.Style                as Style
import           NodeEditor.React.View.Visualization        (nodeVisualization_)
import           Numeric                                    (showFFloat)
import           React.Flux                                 hiding (transform)
import qualified React.Flux                                 as React


name :: JSString
name = "node-editor"

objDynStyle :: JSString
objDynStyle = "dynamic-style"

keyDynScale, keyDynTranslate, keyDynTransform :: JSString
keyDynScale = "dynamic-scale"
keyDynTranslate = "dynamic-translate"
keyDynTransform = "dynamic-transform"

show1 :: Double -> String
show1 a = showFFloat (Just 1) a "" -- limit Double to two decimal numbers TODO: remove before the release

show4 :: Double -> String
show4 a = showFFloat (Just 4) a "" -- limit Double to two decimal numbers TODO: remove before the release

applySearcherHints :: NodeEditor -> NodeEditor
applySearcherHints ne = maybe ne replaceNode $ ne ^. NodeEditor.searcher where
    connect srcPortRef dstPortRef ne' = ne' & NodeEditor.connections . at dstPortRef ?~ Connection.Connection srcPortRef dstPortRef False Connection.Normal
    tryConnect    nl nn ne'           = maybe ne' (\srcPortRef -> connect srcPortRef (InPortRef nl [Self]) ne') $ nn ^. Searcher.predPortRef
    toModel       n  nl pos           = moveNodeToTop $ (convert (def :: NodePath, n)) & ExpressionNode.nodeLoc  .~ nl
                                                                                       & ExpressionNode.position .~ pos
    updateNode    nl n ne'            = maybe ne' (flip NodeEditor.updateExpressionNode ne . Searcher.applyExpressionHint n) $ NodeEditor.getExpressionNode nl ne'
    moveNodeToTop n                   = n & ExpressionNode.zPos .~ (ne ^. NodeEditor.topZIndex) + 1
    replaceNode   s                   = case (s ^. Searcher.mode, s ^. Searcher.selectedNode) of
        (Searcher.Node nl (Searcher.NodeModeInfo _ Nothing   _ _) _, Just n) -> updateNode nl n ne
        (Searcher.Node nl (Searcher.NodeModeInfo _ (Just nn) _ _) _, Just n) -> tryConnect nl nn $ NodeEditor.updateExpressionNode (toModel n nl (nn ^. Searcher.position)) ne
        (Searcher.Node nl (Searcher.NodeModeInfo _ (Just nn) _ _) _, _)      -> tryConnect nl nn $ NodeEditor.updateExpressionNode (moveNodeToTop $ ExpressionNode.mkExprNode nl (s ^. Searcher.inputText) (nn ^. Searcher.position)) ne
        _                                                                    -> ne

nodeEditor_ :: IsRef r => r -> NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref ne = React.viewWithSKey nodeEditor name (ref, ne) mempty

nodeEditor :: IsRef r => ReactView (r, NodeEditor)
nodeEditor = React.defineView name $ \(ref, ne') -> do
    let ne               = applySearcherHints ne'
        camera           = ne ^. NodeEditor.screenTransform . CameraTransformation.logicalToScreen
        nodes            = ne ^. NodeEditor.expressionNodes . to HashMap.elems
        input            = ne ^. NodeEditor.inputNode
        output           = ne ^. NodeEditor.outputNode
        lookupNode m     = ( m ^. MonadPath.monadType
                           , m ^. MonadPath.path . to (mapMaybe $ flip HashMap.lookup $ ne ^. NodeEditor.expressionNodes))
        monads           = map lookupNode $ ne ^. NodeEditor.monads
        visLibPath       = ne ^. NodeEditor.visualizersLibPath
        maybeSearcher    = (,visLibPath) <$> ne ^. NodeEditor.searcher
        visualizations   = NodeEditor.getVisualizations ne
        isAnyVisActive   = any (\visProp -> elem (visProp ^. visPropVisualization . visualizationMode) [Preview, FullScreen, Focused]) visualizations
        isAnyFullscreen  = any (\visProp -> elem (visProp ^. visPropVisualization . visualizationMode) [Preview, FullScreen]) visualizations
        nodesWithVis     = Set.fromList $ map (^. visPropNodeLoc) visualizations
        visWithSelection = map (\vis -> (vis, NodeEditor.isVisualizationNodeSelected vis ne)) visualizations
        mayEditedTextPortControlPortRef = ne ^. NodeEditor.textControlEditedPortRef
    case ne ^. NodeEditor.graphStatus of
        GraphLoaded ->
            div_ [ "className" $= Style.prefixFromList ( ["studio-window"]
                                                       <> if isAnyFullscreen          then ["studio-window--has-visualization-fullscreen"] else []
                                                       <> if maybeSearcher /= Nothing then ["studio-window--has-searcher"]                 else []
                                                       )
                 , "key" $= "studio-window"] $ do

                div_ [ "className" $= Style.prefix "studio-window__center", "key" $= "studio-window__center" ] $
                    div_
                        [ "className" $= Style.prefixFromList (["graph"] <> if isAnyVisActive  then ["graph--has-visualization-active"] else [])
                        , "key"       $= "graph"
                        ] $ do

                        dynamicStyles_ camera $ ne ^. NodeEditor.expressionNodesRecursive

                        planeMonads_ $
                            monads_ monads

                        planeNodes_ $ do

                            forM_ nodes $ \n -> node_ ref
                                                      n
                                                      (not . null $ ne ^. NodeEditor.posHalfConnections)
                                                      (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maybeSearcher)
                                                      (filterOutEditedTextControlIfNotRelated (n ^. Node.nodeLoc) mayEditedTextPortControlPortRef)
                                                      (Set.filter (ExpressionNode.containsNode (n ^. Node.nodeLoc)) nodesWithVis)
                            planeConnections_ $ do
                                forM_ (ne ^. NodeEditor.posConnections ) $ connection_ ref
                                forM_ (ne ^. NodeEditor.selectionBox   ) selectionBox_
                                forM_ (ne ^. NodeEditor.connectionPen  ) connectionPen_

                            forM_ visWithSelection . uncurry $ nodeVisualization_ ref visLibPath


                        planeNewConnection_ $ do
                            forKeyed_ (ne ^. NodeEditor.posHalfConnections) $ uncurry halfConnection_

                withJust input  $ \n -> sidebar_ ref (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maybeSearcher) n
                withJust output $ sidebar_ ref Nothing

                planeCanvas_ mempty --required for cursor lock

        GraphLoading   -> noGraph_ True  "Loading…"
        NoGraph        -> noGraph_ False ""
        GraphError msg -> noGraph_ True  msg

noGraph_ :: Bool -> String -> ReactElementM ViewEventHandler ()
noGraph_ hideLogo msg =
    div_ [ "className" $= Style.prefix "graph"] $
        div_ [ "className" $= Style.prefix "background-text"] $ do
            unless hideLogo $ div_ [ "className" $= Style.prefix "message-logo" ] mempty
            elemString msg

dynamicStyles_ :: Matrix Double -> [ExpressionNode] -> ReactElementM ViewEventHandler ()
dynamicStyles_ camera nodes = React.viewWithSKey dynamicStyles "dynamic-styles" (camera, nodes) mempty

dynamicStyles :: ReactView (Matrix Double, [ExpressionNode])
dynamicStyles = React.defineView "dynamic-styles" $ \(camera, nodes) -> do
    dynamicTransform_ camera
    dynamicTranslate_ $ convert camera
    dynamicScale_ $ convert camera
    forM_ nodes $ nodeDynamicStyles_ camera

dynamicScale_ :: CameraScale -> ReactElementM ViewEventHandler ()
dynamicScale_ cameraScale = React.viewWithSKey dynamicScale keyDynScale cameraScale mempty

dynamicScale :: ReactView CameraScale
dynamicScale = React.defineView objDynStyle $ \cameraScale -> do
    let scale = cameraScale ^. Matrix.scale
    style_
        [ "key" $= "scale"
        ] $ do
          --camera
          elemString $ ":root { font-size: " <> show scale <> "px !important }"
          --elemString $ ".luna-camera-scale { transform: " <> showCameraScale cameraScale <> " }"

          --connections
          elemString $ ".luna-connection__line, .luna-port--alias .luna-port__shape { stroke-width: "   <> show (1.2 + (1 / scale)) <> " }"
          elemString $ ".luna-connection__select { stroke-width: " <> show (10/scale)          <> " }"

          --collapsed nodes
          elemString $ ".luna-port-io-shape-mask { r: "  <> show (19.2 + (0.8 / scale)) <> "px }"
          elemString $ ".luna-port-io-select-mask { r: " <> show (19.2 + (0.8 / scale)) <> "px }"

          --expanded nodes
          elemString $ "circle.luna-port__shape { r: " <> show (3 + (1 / scale)) <> "px }"
          elemString $ ".luna-port--alias circle.luna-port__shape { r: " <> show (7 + (1 / scale)) <> "px }"

dynamicTranslate_ :: CameraTranslate -> ReactElementM ViewEventHandler ()
dynamicTranslate_ cameraTranslate = React.viewWithSKey dynamicTranslate keyDynTranslate cameraTranslate mempty

dynamicTranslate :: ReactView CameraTranslate
dynamicTranslate = React.defineView objDynStyle $ \cameraTranslate ->
    style_
        [ "key" $= "translate"
        ] $ elemString $ ".luna-camera-translate { transform: " <> showCameraTranslate cameraTranslate <> " }"

dynamicTransform_ :: Matrix Double -> ReactElementM ViewEventHandler ()
dynamicTransform_ camera = React.viewWithSKey dynamicTransform keyDynTransform camera mempty

dynamicTransform :: ReactView (Matrix Double)
dynamicTransform = React.defineView objDynStyle $ \camera ->
    style_
        [ "key" $= "transform"
        ] $ elemString $ ".luna-camera-transform { transform: " <> showCameraMatrix camera <> " }"
