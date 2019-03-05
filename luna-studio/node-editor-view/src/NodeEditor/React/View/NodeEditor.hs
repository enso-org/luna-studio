module NodeEditor.React.View.NodeEditor where

import Common.Prelude hiding (transform)
import React.Flux     hiding (transform)

import qualified Data.HashMap.Strict                        as HashMap
import qualified Data.Set                                   as Set
import qualified LunaStudio.Data.CameraTransformation       as CameraTransformation
import qualified LunaStudio.Data.Matrix                     as Matrix
import qualified LunaStudio.Data.MonadPath                  as MonadPath
import qualified NodeEditor.React.Model.Connection          as Connection
import qualified NodeEditor.React.Model.Node                as Node
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified NodeEditor.React.Model.Node.SidebarNode    as SidebarNode
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import qualified NodeEditor.React.Model.Searcher            as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint       as Hint
import qualified NodeEditor.React.Model.Searcher.Hint.Node  as Hint
import qualified NodeEditor.React.Model.Searcher.Mode       as SearcherMode
import qualified NodeEditor.React.Model.Searcher.Mode.Node  as NodeSearcher
import qualified NodeEditor.React.View.Sidebar              as Sidebar
import qualified NodeEditor.React.View.Style                as Style
import qualified React.Flux                                 as React
import qualified Searcher.Data.Result                       as Result

import Data.Matrix                                (Matrix)
import Data.Maybe                                 (mapMaybe)
import LunaStudio.Data.Error                      (errorContent)
import LunaStudio.Data.Matrix                     (CameraScale, CameraTranslate,
                                                   showCameraMatrix,
                                                   showCameraTranslate)
import LunaStudio.Data.NodeLoc                    (NodeLoc, NodePath)
import LunaStudio.Data.PortRef                    (InPortRef (InPortRef))
import NodeEditor.React.IsRef                     (IsRef)
import NodeEditor.React.Model.Constants           (nodeRadius)
import NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, mkExprNode)
import NodeEditor.React.Model.NodeEditor          (GraphStatus (..), NodeEditor)
import NodeEditor.React.Model.Port                (InPortIndex (Self))
import NodeEditor.React.Model.Visualization       (VisualizationMode (Focused, FullScreen, Preview),
                                                   visPropNodeLoc,
                                                   visPropVisualization,
                                                   visualizationMode)
import NodeEditor.React.View.Connection           (connection_, halfConnection_)
import NodeEditor.React.View.ConnectionPen        (connectionPen_)
import NodeEditor.React.View.ExpressionNode       (filterOutEditedTextControlIfNotRelated,
                                                   filterOutSearcherIfNotRelated,
                                                   nodeDynamicStyles_, node_)
import NodeEditor.React.View.Monad                (monads_)
import NodeEditor.React.View.Plane                (planeCanvas_,
                                                   planeConnections_,
                                                   planeMonads_,
                                                   planeNewConnection_,
                                                   planeNodes_)
import NodeEditor.React.View.SelectionBox         (selectionBox_)
import NodeEditor.React.View.Sidebar              (sidebar_)
import NodeEditor.React.View.Visualization        (nodeVisualization_)
import Numeric                                    (showFFloat)


data NoGraphMode = LoadingMode | EmptyMode | ErrorMode deriving Eq

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

mockSearcherNode :: NodeEditor -> NodeEditor
mockSearcherNode ne = maybe ne withSearcher maySearcher where
    maySearcher = ne ^? NodeEditor.searcherProperties . _Just . Searcher.searcher
    withSearcher s = let
        mayNodeSearcher = s ^? Searcher.mode . SearcherMode._Node
        in maybe ne (withNodeSearcher s) mayNodeSearcher
    withNodeSearcher s ns = let
        mayNewNodeData = ns ^? NodeSearcher.mode 
            . NodeSearcher._ExpressionMode . NodeSearcher.newNode . _Just
        nl   = ns ^. NodeSearcher.nodeLoc
        hint = s ^? Searcher.selectedResult . _Just . Result.hint . Hint._Node
        in maybe (updateNode nl hint) (mockNewNode nl hint) mayNewNodeData
    updateNode :: NodeLoc -> Maybe Hint.Node -> NodeEditor
    updateNode nl mayHint = maybe
        ne
        (uncurry updateWithHint)
        $ (,)
            <$> NodeEditor.getExpressionNode nl ne
            <*> mayHint
    updateWithHint
        :: ExpressionNode
        -> Hint.Node
        -> NodeEditor
    updateWithHint n hint
        = NodeEditor.updateExpressionNode (applyHint n hint) ne
    mockNewNode
        :: NodeLoc
        -> Maybe Hint.Node
        -> NodeSearcher.New
        -> NodeEditor
    mockNewNode nl mayHint newNodeData = do
        let defNode = mkExprNode nl def (newNodeData ^. NodeSearcher.position)
            node = maybe defNode (applyHint defNode) mayHint
            maySrcPortRef = newNodeData ^. NodeSearcher.connectionSource
            dstPortRef = InPortRef nl [Self]
            mkConnection srcPortRef = Connection.Connection
                srcPortRef
                dstPortRef
                False
                Connection.Normal
            mayConnection = mkConnection <$> maySrcPortRef
        NodeEditor.updateExpressionNode node ne
            & NodeEditor.connections . at dstPortRef .~ mayConnection
    applyHint :: ExpressionNode -> Hint.Node -> ExpressionNode
    applyHint n hint = n --TODO[LJK]: Mock new node here once searcher knows about ports

nodeEditor_ :: IsRef r => r -> NodeEditor -> Bool -> ReactElementM ViewEventHandler ()
nodeEditor_ ref ne isTopLevel = React.viewWithSKey nodeEditor name (ref, ne, isTopLevel) mempty

nodeEditor :: IsRef r => ReactView (r, NodeEditor, Bool)
nodeEditor = React.defineView name $ \(ref, ne, isTopLevel) -> do
    case ne ^. NodeEditor.graphStatus of
        GraphLoading -> noGraph_ LoadingMode "Loading…"
        NoGraph      -> noGraph_ EmptyMode ""
        GraphLoaded  -> div_ ["className" $= Style.prefix "graph-container" ] $ graph_ ref ne isTopLevel
        GraphError e -> div_ ["className" $= Style.prefixFromList [ "graph-container", "graph-container--error" ] ] $ do
            graph_ ref ne isTopLevel
            div_ ["className" $= Style.prefix "graph-error"] $ div_ ["className" $= Style.prefix "graph-error__message"] $ elemString $ convert $ e ^. errorContent


graph_ :: IsRef r => r -> NodeEditor -> Bool -> ReactElementM ViewEventHandler ()
graph_ ref ne isTopLevel = React.viewWithSKey graph name (ref, ne, isTopLevel) mempty

graph :: IsRef r => ReactView (r, NodeEditor, Bool)
graph = React.defineView name $ \(ref, (mockSearcherNode -> ne), isTopLevel) -> do
    let camera           = ne ^. NodeEditor.screenTransform . CameraTransformation.logicalToScreen
        nodes            = ne ^. NodeEditor.expressionNodes . to HashMap.elems
        input            = ne ^. NodeEditor.inputNode
        output           = ne ^. NodeEditor.outputNode
        lookupNode m     = ( m ^. MonadPath.monadType
                           , m ^. MonadPath.path . to (mapMaybe $ flip HashMap.lookup $ ne ^. NodeEditor.expressionNodes))
        monads           = map lookupNode $ ne ^. NodeEditor.monads
        visLibPaths      = ne ^. NodeEditor.visualizersLibPaths
        visualizations   = NodeEditor.getVisualizations ne
        isAnyVisActive   = any (\visProp -> elem (visProp ^. visPropVisualization . visualizationMode) [Preview, FullScreen, Focused]) visualizations
        isAnyFullscreen  = any (\visProp -> elem (visProp ^. visPropVisualization . visualizationMode) [Preview, FullScreen]) visualizations
        nodesWithVis     = Set.fromList $ map (^. visPropNodeLoc) visualizations
        visWithSelection = map (\vis -> (vis, NodeEditor.isVisualizationNodeSelected vis ne)) visualizations
        mayEditedTextPortControlPortRef = ne ^. NodeEditor.textControlEditedPortRef
        allowVisualizations             = not isTopLevel && hasn't (NodeEditor.inputNode . _Just . SidebarNode.inputSidebarPorts . traverse) ne
    div_
        [ "className" $= Style.prefixFromList
            ( ["studio-window"]
            <> if allowVisualizations && isAnyFullscreen
                then ["studio-window--has-visualization-fullscreen"]
                else mempty
            <> if isJust $ ne ^. NodeEditor.searcher
                then ["studio-window--has-searcher"]
                else mempty)
         , "key" $= "studio-window"]
         $ do
            let graphDiv content = div_
                    [ "className" $= Style.prefix "studio-window__center"
                    , "key" $= "studio-window__center" ] $
                    div_
                        [ "className" $= Style.prefixFromList
                            (["graph"]
                            <> if allowVisualizations && isAnyVisActive
                                then ["graph--has-visualization-active"]
                                else mempty)
                        , "key" $= "graph" ]
                        content
            graphDiv $ do
                when (null nodes) $ div_ $ elemString $ "Press TAB to start"
                dynamicStyles_ camera $ ne ^. NodeEditor.expressionNodesRecursive
                planeMonads_ $ monads_ monads
                planeNodes_ $ do
                    forM_ nodes $ \n -> node_
                        ref
                        n
                        isTopLevel
                        (not . null $ ne ^. NodeEditor.posHalfConnections)
                        (filterOutSearcherIfNotRelated
                            (n ^. Node.nodeLoc)
                            (ne ^. NodeEditor.searcherProperties))
                        (filterOutEditedTextControlIfNotRelated
                            (n ^. Node.nodeLoc)
                            mayEditedTextPortControlPortRef)
                        (Set.filter
                            (ExpressionNode.containsNode (n ^. Node.nodeLoc))
                            nodesWithVis)
                        (not allowVisualizations)
                    planeConnections_ $ do
                        forM_ (ne ^. NodeEditor.posConnections)
                            $ connection_ ref
                        forM_ (ne ^. NodeEditor.selectionBox) selectionBox_
                        forM_ (ne ^. NodeEditor.connectionPen) connectionPen_
                    when allowVisualizations . forM_
                        visWithSelection
                        . uncurry $ nodeVisualization_ ref visLibPaths
                planeNewConnection_ $ forKeyed_
                    (ne ^. NodeEditor.posHalfConnections)
                    $ uncurry halfConnection_
            let maySearcher = ne ^. NodeEditor.searcherProperties
                maySidebarSearcher
                    = if has
                        (_Just . Searcher.searcher . Searcher.mode
                            . SearcherMode._Node . NodeSearcher.mode
                            . NodeSearcher._PortNameMode)
                        maySearcher
                            then maySearcher
                            else Nothing

            withJust input  $ sidebar_ ref maySidebarSearcher
            withJust output $ sidebar_ ref Nothing

            planeCanvas_ mempty --required for cursor lock

noGraph_ :: NoGraphMode -> String -> ReactElementM ViewEventHandler ()
noGraph_ mode msg =
    div_ [ "className" $= Style.prefix "graph"] $
        case mode of
            EmptyMode   -> mempty
            LoadingMode -> div_ [ "className" $= Style.prefix "background-text"] $  elemString msg
            ErrorMode   -> div_ [ "className" $= Style.prefix "background-text-container"] $
                               div_ [ "className" $= Style.prefix "background-text"] $ elemString msg

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
        opticalCorrection = 0.8
    style_
        [ "key" $= "scale"
        ] $ do
          --camera
          elemString $ ":root { font-size: " <> show scale <> "px !important }"
          --elemString $ ".luna-camera-scale { transform: " <> showCameraScale cameraScale <> " }"

          --selection
          elemString $ ".luna-selection { stroke-width: " <> show (1/scale) <> "px !important }"

          --connections
          elemString $ ".luna-connection__line, .luna-port--alias .luna-port__shape { stroke-width: " <> show (1.2 + (1 / scale)) <> " }"
          elemString $ ".luna-connection__select { stroke-width: " <> show (10/scale) <> " }"

          --collapsed nodes
          elemString $ ".luna-port-io-shape-mask  { r: " <> show (nodeRadius - opticalCorrection + (opticalCorrection / scale)) <> "px }"
          elemString $ ".luna-port-io-select-mask { r: " <> show (nodeRadius - opticalCorrection + (opticalCorrection / scale)) <> "px }"

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
