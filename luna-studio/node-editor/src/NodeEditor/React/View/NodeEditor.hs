{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.NodeEditor where

import           Common.Prelude                             hiding (transform)
import qualified Data.HashMap.Strict                        as HashMap
import           Data.Matrix                                (Matrix)
import           Data.Maybe                                 (mapMaybe)
import qualified Data.Set                                   as Set
import qualified LunaStudio.Data.CameraTransformation       as CameraTransformation
import           LunaStudio.Data.Matrix                     (CameraScale, CameraTranslate, showCameraMatrix, showCameraScale,
                                                             showCameraTranslate)
import qualified LunaStudio.Data.Matrix                     as Matrix
import qualified LunaStudio.Data.MonadPath                  as MonadPath
import           LunaStudio.Data.NodeLoc                    (NodePath)
import           LunaStudio.Data.PortRef                    (InPortRef (InPortRef))
import           NodeEditor.React.Model.App                 (App)
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
import           NodeEditor.React.Store                     (Ref)
import           NodeEditor.React.View.Connection           (connection_, halfConnection_)
import           NodeEditor.React.View.ConnectionPen        (connectionPen_)
import           NodeEditor.React.View.ExpressionNode       (filterOutSearcherIfNotRelated, nodeDynamicStyles_, node_)
import           NodeEditor.React.View.Monad                (monads_)
import           NodeEditor.React.View.Plane                (planeConnections_, planeMonads_, planeNewConnection_, planeNodes_, svgPlane_)
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
    connect srcPortRef dstPortRef ne' = ne' & NodeEditor.connections . at dstPortRef ?~ Connection.Connection srcPortRef dstPortRef Connection.Normal
    tryConnect nl nn ne'              = maybe ne' (\srcPortRef -> connect srcPortRef (InPortRef nl [Self]) ne') $ nn ^. Searcher.predPortRef
    toModel n nl pos                  = moveNodeToTop $ (convert (def :: NodePath, n)) & ExpressionNode.nodeLoc  .~ nl
                                                                                       & ExpressionNode.position .~ pos
    updateNode nl n ne'               = maybe ne' (flip NodeEditor.updateExpressionNode ne . Searcher.applyExpressionHint n) $ NodeEditor.getExpressionNode nl ne'
    moveNodeToTop n                   = n & ExpressionNode.zPos .~ (ne ^. NodeEditor.topZIndex) + 1
    replaceNode s                     = case (s ^. Searcher.mode, s ^. Searcher.selectedNode) of
        (Searcher.Node nl (Searcher.NodeModeInfo _ Nothing   _) _, Just n) -> updateNode nl n ne
        (Searcher.Node nl (Searcher.NodeModeInfo _ (Just nn) _) _, Just n) -> tryConnect nl nn $ NodeEditor.updateExpressionNode (toModel n nl (nn ^. Searcher.position)) ne
        (Searcher.Node nl (Searcher.NodeModeInfo _ (Just nn) _) _, _)      -> tryConnect nl nn $ NodeEditor.updateExpressionNode (moveNodeToTop $ ExpressionNode.mkExprNode nl (s ^. Searcher.inputText) (nn ^. Searcher.position)) ne
        _                                                                  -> ne

nodeEditor_ :: Ref App -> NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref ne = React.viewWithSKey nodeEditor name (ref, ne) mempty

nodeEditor :: ReactView (Ref App, NodeEditor)
nodeEditor = React.defineView name $ \(ref, ne') -> do
    let ne             = applySearcherHints ne'
        camera         = ne ^. NodeEditor.screenTransform . CameraTransformation.logicalToScreen
        nodes          = ne ^. NodeEditor.expressionNodes . to HashMap.elems
        input          = ne ^. NodeEditor.inputNode
        output         = ne ^. NodeEditor.outputNode
        lookupNode m   = ( m ^. MonadPath.monadType
                         , m ^. MonadPath.path . to (mapMaybe $ flip HashMap.lookup $ ne ^. NodeEditor.expressionNodes))
        monads         = map lookupNode $ ne ^. NodeEditor.monads
        maySearcher    = ne ^. NodeEditor.searcher
        visualizations = NodeEditor.getVisualizations ne
        isAnyVisActive = any (\visProp -> elem (visProp ^. visPropVisualization . visualizationMode) [Preview, FullScreen, Focused]) visualizations
        nodesWithVis   = Set.fromList $ map (^. visPropNodeLoc) visualizations
    case ne ^. NodeEditor.graphStatus of
        GraphLoaded ->
            div_ [ "className" $= Style.prefix "studio--window", "key" $= "studio--window"] $ do
                div_ [ "className" $= Style.prefix "studio--window__center", "key" $= "studio--window__center" ] $
                    div_
                        [ "className"   $= Style.prefixFromList (["graph"]++if isAnyVisActive then ["graph--has-visualization-active"] else [])
                        , "key"         $= "graph"
                        ] $ do

                        dynamicStyles_ camera $ ne ^. NodeEditor.expressionNodesRecursive

                        svgPlane_ $ do
                            planeMonads_ $
                                monads_ monads
                            planeConnections_ $ do
                                forM_ (ne ^. NodeEditor.posConnections ) $ connection_ ref
                                forM_ (ne ^. NodeEditor.selectionBox   ) selectionBox_
                                forM_ (ne ^. NodeEditor.connectionPen  ) connectionPen_

                        planeNodes_ $ do
                            forM_ nodes $ \n -> node_ ref
                                                      n
                                                      (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maySearcher)
                                                      (Set.filter (ExpressionNode.containsNode (n ^. Node.nodeLoc)) nodesWithVis)
                            forM_ visualizations $ nodeVisualization_ ref

                        planeNewConnection_ $ do
                            forKeyed_ (ne ^. NodeEditor.posHalfConnections) $ uncurry halfConnection_

                withJust input  $ \n -> sidebar_ ref (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maySearcher) n
                withJust output $ sidebar_ ref Nothing
                --planeCanvas_ mempty

        GraphLoading   -> noGraph_ True "Loading…"
        NoGraph        -> noGraph_ False ""
        GraphError msg -> noGraph_ True msg

noGraph_ :: Bool -> String -> ReactElementM ViewEventHandler ()
noGraph_ hideLogo msg =
    div_ [ "className" $= Style.prefix "graph"] $
        div_ [ "className" $= Style.prefix "background-text"] $ do
            unless hideLogo $ img_
                [ "className" $= Style.prefix "message-logo"
                , "src"       $= "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxODAiIGhlaWdodD0iMTgwIj48ZyBmaWxsPSJub25lIiBmaWxsLXJ1bGU9ImV2ZW5vZGQiPjxwYXRoIGZpbGw9IiM1RjE1MjIgIiBkPSJNMzUuMiA4MC40YzMwLjUgMCA0MC43IDI0IDU3LjMgMjIuNEMxMDkgMTAxIDU2IDEyMy4zIDU2IDEyMy4zUzE4IDEyNyAxOCA5MGMwLTcuNCAxMi43LTkuNiAxNy4yLTkuNnoiLz48cGF0aCBmaWxsPSIjRjIyMTQ2ICIgZD0iTTkwIDE4MGM0OS43IDAgOTAtNDAuMyA5MC05MFMxMzkuNyAwIDkwIDAgMCA0MC4zIDAgOTBzNDAuMyA5MCA5MCA5MHptMC05YzQ0LjcgMCA4MS0zNi4zIDgxLTgxUzEzNC43IDkgOTAgOSA5IDQ1LjMgOSA5MHMzNi4zIDgxIDgxIDgxem03Mi03OS4yYzAgMzktMzIuOCA3MC4yLTcyIDcwLjItMzkuOCAwLTcyLTMyLjItNzItNzIgOSAxNC43IDI0IDI0LjYgNDAuMyAyNS4yIDE5LjQuOCAzOS43LTExIDQ4LjMtMzEuNiA2LTE0LjIgMTYuNy0xOS41IDI3LTE5LjUgMTMuOCAwIDI4LjQgMTEgMjguNCAyOHoiLz48L2c+PC9zdmc+Cg=="
                ] mempty
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
          elemString $ ":root { font-size: " <> show scale <> "px !important }"
          elemString $ ".luna-camera-scale { transform: "     <> showCameraScale cameraScale <> " }"

          elemString $ ".luna-connection__line { stroke-width: "   <> show (1.2 + (1 / scale)) <> " }"
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
