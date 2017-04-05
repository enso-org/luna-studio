{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.ExpressionNode where

import qualified Data.Aeson                                            as Aeson
import qualified Data.HashMap.Strict                                   as HashMap
import qualified Data.Map.Lazy                                         as Map
import           Data.Matrix                                           (Matrix)
import qualified Empire.API.Data.MonadPath                             as MonadPath
import           Empire.API.Data.PortRef                               (toAnyPortRef)
import qualified Empire.API.Graph.NodeResultUpdate                     as NodeResult
import qualified JS.Config                                             as Config
import qualified JS.UI                                                 as UI
import           Luna.Studio.Data.Matrix                               (showNodeMatrix, showNodeTranslate)
import qualified Luna.Studio.Event.Mouse                               as Mouse
import qualified Luna.Studio.Event.UI                                  as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node                          as Node
import           Luna.Studio.React.Model.App                           (App)
import qualified Luna.Studio.React.Model.Field                         as Field
import           Luna.Studio.React.Model.Node.ExpressionNode           (ExpressionNode, NodeLoc, Subgraph, countArgPorts, countOutPorts,
                                                                        isCollapsed, returnsError)
import qualified Luna.Studio.React.Model.Node.ExpressionNode           as Node
import qualified Luna.Studio.React.Model.Node.ExpressionNodeProperties as Prop
import           Luna.Studio.React.Model.Port                          (InPort (Arg, Self), PortId (InPortId), isAll, isInPort)
import qualified Luna.Studio.React.Model.Port                          as Port
import           Luna.Studio.React.Store                               (Ref, dispatch)
import           Luna.Studio.React.View.ExpressionNode.Properties      (nodeProperties_)
import           Luna.Studio.React.View.Field                          (singleField_)
import           Luna.Studio.React.View.Field                          (multilineField_)
import           Luna.Studio.React.View.Monad                          (monads_)
import           Luna.Studio.React.View.Plane                          (planeMonads_, svgPlanes_)
import           Luna.Studio.React.View.Port                           (portExpanded_, portPhantom_, port_)
import           Luna.Studio.React.View.Style                          (blurBackground_, errorMark_, selectionMark_)
import qualified Luna.Studio.React.View.Style                          as Style
import           Luna.Studio.React.View.Visualization                  (strValue, visualization_)
import           React.Flux
import qualified React.Flux                                            as React


name, objNameBody, objNameVis, objNamePorts :: JSString
name         = "node"
objNameBody  = "node-body"
objNameVis   = "node-vis"
objNamePorts = "node-ports"

nodePrefix :: JSString
nodePrefix = Config.prefix "node-"

nameLabelId :: JSString
nameLabelId = Config.prefix "focus-nameLabel"

focusNameLabel :: IO ()
focusNameLabel = UI.focus nameLabelId

handleMouseDown :: Ref App -> NodeLoc -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref nodeLoc e m =
    if Mouse.withoutMods m Mouse.leftButton || Mouse.withShift m Mouse.leftButton
    then stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeLoc)
    else []

node_ :: Ref App -> ExpressionNode -> ReactElementM ViewEventHandler ()
node_ ref model = React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model) mempty

node :: ReactView (Ref App, ExpressionNode)
node = React.defineView name $ \(ref, n) -> case n ^. Node.mode of
    Node.Expanded (Node.Function fs) -> nodeContainer_ ref $ Map.elems fs
    _ ->
        let nodeId    = n ^. Node.nodeId
            nodeLoc   = n ^. Node.nodeLoc
            nodeLimit = 10000::Int
            zIndex    = n ^. Node.zPos
            z         = if isCollapsed n then zIndex else zIndex + nodeLimit
        in div_
            [ "key"       $= (nodePrefix <> fromString (show nodeId))
            , "id"        $= (nodePrefix <> fromString (show nodeId))
            , "className" $= Style.prefixFromList ( [ "node", (if isCollapsed n then "node--collapsed" else "node--expanded") ]
                                                           ++ (if returnsError n then ["node--error"] else [])
                                                           ++ (if n ^. Node.isSelected then ["node--selected"] else []) )
            , "style"     @= Aeson.object [ "zIndex" Aeson..= show z ]
            , onMouseDown   $ handleMouseDown ref nodeLoc
            , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeLoc
            , onDoubleClick $ \e _ -> stopPropagation e : (dispatch ref $ UI.NodeEvent $ Node.Enter nodeLoc)
            ] $ do
            svg_
                [ "className" $= Style.prefix "node__text"
                , "key"       $= "nodeText"
                ] $
                g_
                    [ "className" $= Style.prefix "node-translate"
                    ] $ do
                    text_
                        [ "key"         $= "expressionText"
                        , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeLoc)
                        , "className"   $= Style.prefixFromList [ "node__name", "node__name--expression", "noselect" ]
                        , "y"           $= "-16"
                        ] $ elemString . convert $ n ^. Node.expression

                    if n ^. Node.isNameEdited then
                        term "foreignObject"
                            [ "key"    $= "nameEdit"
                            , "width"  $= "200"
                            , "height" $= "30"
                            ] $ singleField_ ["id"  $= nameLabelId] "name-label"
                                $ Field.mk ref (n ^. Node.name)
                                & Field.onCancel .~ Just (const $ UI.NodeEvent $ Node.NameEditDiscard nodeLoc)
                                & Field.onAccept .~ Just (UI.NodeEvent . Node.NameEditApply nodeLoc)
                    else
                        text_
                            [ "key"         $= "nameText"
                            , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.NameEditStart nodeLoc)
                            , "className"   $= Style.prefixFromList [ "node__name", "noselect" ]
                            ] $ elemString $ convert $ n ^. Node.name
                    g_
                        [ "key"       $= "icons"
                        , "className" $= Style.prefix "node__icons"
                        ] $ do
                        let val = Prop.fromNode n ^. Prop.visualizationsEnabled
                        rect_
                            [ "key" $= "ctrlSwitch"
                            , "className" $= Style.prefixFromList (["icon", "icon--show"] ++ if val then ["icon--show--on"] else ["icon--show--off"])
                            , onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.DisplayResultChanged (not val) nodeLoc
                            ] mempty
            nodeBody_ ref n
            nodeVisualizations_ ref n
            nodePorts_ ref n

nodeDynamicStyles_ :: Matrix Double -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeDynamicStyles_ camera n = do
    let nodeId  = n ^. Node.nodeId
        nodePos = n ^. Node.position
    elemString $ "#" <> Config.mountPoint <> "-node-" <> fromString (show nodeId) <> " .luna-node-translate--name { transform: " <> showNodeTranslate camera nodePos <> " }"
    elemString $ "#" <> Config.mountPoint <> "-node-" <> fromString (show nodeId) <> " .luna-node-translate { transform: "       <> showNodeTranslate camera nodePos <> " }"
    elemString $ "#" <> Config.mountPoint <> "-node-" <> fromString (show nodeId) <> " .luna-node-transform { transform: "       <> showNodeMatrix    camera nodePos <> " }"
    elemString $ "#" <> Config.mountPoint <> "-node-" <> fromString (show nodeId) <> " path.luna-port__shape { clip-path: url(#port-io-shape-mask-"   <> show nodeId <> ") }"
    elemString $ "#" <> Config.mountPoint <> "-node-" <> fromString (show nodeId) <> " path.luna-port__select { clip-path: url(#port-io-select-mask-" <> show nodeId <> ") }"

nodeBody_ :: Ref App -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeBody_ ref model = React.viewWithSKey nodeBody "node-body" (ref, model) mempty

nodeBody :: ReactView (Ref App, ExpressionNode)
nodeBody = React.defineView objNameBody $ \(ref, n) ->
    let nodeLoc = n ^. Node.nodeLoc
    in div_
        [ "key"       $= "nodeBody"
        , "className" $= Style.prefixFromList [ "node__body", "node-translate" ]
        ] $ do
        errorMark_
        selectionMark_
        div_
            [ "key"       $= "properties-crop"
            , "className" $= Style.prefix "node__properties-crop"
            ] $ do
            blurBackground_
            case n ^. Node.mode of
                Node.Expanded Node.Controls      -> nodeProperties_ ref $ Prop.fromNode n
                Node.Expanded Node.Editor        -> multilineField_ [] "editor"
                    $ Field.mk ref (fromMaybe def $ n ^. Node.code)
                    & Field.onCancel .~ Just (UI.NodeEvent . Node.SetCode nodeLoc)
                _                                -> ""

nodeVisualizations_ :: Ref App -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeVisualizations_ ref model = React.viewWithSKey nodeVisualizations objNameVis (ref, model) mempty

nodeVisualizations :: ReactView (Ref App, ExpressionNode)
nodeVisualizations = React.defineView objNameVis $ \(ref, n) ->
    let nodeLoc = n ^. Node.nodeLoc
    in div_
        [ "key"       $= "shortValue"
        , "className" $= Style.prefixFromList [ "node__returned-value", "node-translate", "noselect" ]
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $ elemString $ strValue n
--    div_
--        [ "key"       $= "visualizations"
--        , "className" $= Style.prefixFromList [ "node__visualisations", "node-translate" ]
--        ] $ forM_ (n ^. Node.value) $ visualization_ ref nodeLoc def

nodePorts_ :: Ref App -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodePorts_ ref model = React.viewWithSKey nodePorts objNamePorts (ref, model) mempty

nodePorts :: ReactView (Ref App, ExpressionNode)
nodePorts = React.defineView objNamePorts $ \(ref, n) ->
    let nodeId     = n ^. Node.nodeId
        nodeLoc    = n ^. Node.nodeLoc
        nodePorts' = Map.elems $ n ^. Node.ports
        ports p   = forM_ p $ \port -> port_ ref
                                             nodeLoc
                                             port
                                            (if isInPort $ port ^. Port.portId then countArgPorts n else countOutPorts n)
                                            (isAll (port ^. Port.portId) && countArgPorts n + countOutPorts n == 1)

    in svg_
        [ "key"       $= "nodePorts"
        , "className" $= Style.prefixFromList [ "node__ports" ]
        ] $ do
        defs_
            [ "key" $= "defs" ] $ do
            clipPath_
                [ "id"  $= fromString ("port-io-shape-mask-" <> show nodeId)
                , "key" $= "portIoShapeMask"
                ] $ do
                circle_
                    [ "className" $= Style.prefix "port-io-shape-mask"
                    ] mempty
            clipPath_
                [ "id"  $= fromString ("port-io-shape-mask-" <> show nodeId)
                , "key" $= "portIoSelectMask"
                ] $
                circle_
                    [ "className" $= Style.prefix "port-io-select-mask"
                    ] mempty
        g_
            [ "className" $= Style.prefix "node-transform"
            , "key"       $= "nodeTransform"
            ] $ do
            if isCollapsed n then do
                ports $ filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts'
                ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts'
            else do
                ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts'
                forM_  (filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts') $ \port -> portExpanded_ ref nodeLoc port
            portPhantom_ ref $ toAnyPortRef nodeLoc $ InPortId (Arg $ countArgPorts n)

nodeContainer_ :: Ref App -> [Subgraph] -> ReactElementM ViewEventHandler ()
nodeContainer_ ref subgraphs = React.viewWithSKey nodeContainer "node-container" (ref, subgraphs) mempty

nodeContainer :: ReactView (Ref App, [Subgraph])
nodeContainer = React.defineView name $ \(ref, subgraphs) -> do
    div_
        [ "className" $= Style.prefix "subgraphs"
        ] $ forM_ subgraphs $ \subgraph -> do
        let sidebars     = subgraph ^. Node.sidebarNodes . to HashMap.elems
            nodes        = subgraph ^. Node.expressionNodes . to HashMap.elems
            lookupNode m = ( m ^. MonadPath.monadType
                           , m ^. MonadPath.path . to (mapMaybe $ flip HashMap.lookup $ subgraph ^. Node.expressionNodes))
            monads       = map lookupNode $ subgraph ^. Node.monads
        div_
            [ "className" $= Style.prefix "subgraph"
            ] $ do
            forM_ nodes $ node_ ref
            svgPlanes_ $ planeMonads_ $ monads_ monads
