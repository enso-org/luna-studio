{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode where

import           Common.Prelude
import qualified Data.Aeson                                           as Aeson
import qualified Data.HashMap.Strict                                  as HashMap
import qualified Data.Map.Lazy                                        as Map
import           Data.Matrix                                          (Matrix)
import qualified JS.Config                                            as Config
import qualified JS.UI                                                as UI
import           LunaStudio.Data.LabeledTree                          (LabeledTree (LabeledTree))
import qualified LunaStudio.Data.MonadPath                            as MonadPath
import           LunaStudio.Data.PortRef                              (toAnyPortRef)
import           NodeEditor.Data.Matrix                               (showNodeMatrix, showNodeTranslate)
import qualified NodeEditor.Event.Mouse                               as Mouse
import qualified NodeEditor.Event.UI                                  as UI
import qualified NodeEditor.React.Event.Node                          as Node
import           NodeEditor.React.Model.App                           (App)
import qualified NodeEditor.React.Model.Field                         as Field
import           NodeEditor.React.Model.Node.ExpressionNode           (ExpressionNode, NodeLoc, Subgraph, countArgPorts, countOutPorts,
                                                                       isCollapsed, returnsError)
import qualified NodeEditor.React.Model.Node.ExpressionNode           as Node
import qualified NodeEditor.React.Model.Node.ExpressionNodeProperties as Prop
import           NodeEditor.React.Model.Port                          (AnyPortId (InPortId'), InPortIndex (Arg, Self), isInPort, isOutAll,
                                                                       withOut)
import qualified NodeEditor.React.Model.Port                          as Port
import           NodeEditor.React.Model.Searcher                      (Searcher)
import qualified NodeEditor.React.Model.Searcher                      as Searcher
import           NodeEditor.React.Store                               (Ref, dispatch)
import           NodeEditor.React.View.ExpressionNode.Properties      (nodeProperties_)
import           NodeEditor.React.View.Field                          (multilineField_)
import           NodeEditor.React.View.Monad                          (monads_)
import           NodeEditor.React.View.Plane                          (planeMonads_, svgPlanes_)
import           NodeEditor.React.View.Port                           (portExpanded_, portPhantom_, port_)
import           NodeEditor.React.View.Searcher                       (searcher_)
import           NodeEditor.React.View.Style                          (errorMark_, selectionMark_)
import qualified NodeEditor.React.View.Style                          as Style
import           NodeEditor.React.View.Visualization                  (nodeShortValue_, nodeVisualizations_)
import           React.Flux
import qualified React.Flux                                           as React


name, objNameBody, objNamePorts :: JSString
name            = "node"
objNameBody     = "node-body"
objNamePorts    = "node-ports"

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

nodeName_ :: Ref App -> NodeLoc -> Maybe Text -> Maybe Searcher -> ReactElementM ViewEventHandler ()
nodeName_ ref nl nodeName mayS = div_ ([ "className" $= Style.prefixFromList ["node__name", "noselect"] ] ++ handlers) nameElement where
    regularHandlersAndElem = ( [onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditName nl)]
                             , elemString . convert $ fromMaybe def nodeName )
    (handlers, nameElement) = flip (maybe regularHandlersAndElem) mayS $ \s -> case s ^. Searcher.mode of
        Searcher.NodeName snl _ -> if snl /= nl then regularHandlersAndElem else ([], searcher_ ref s)
        _                       -> regularHandlersAndElem

nodeExpression_ :: Ref App -> NodeLoc -> Text -> Maybe Searcher -> ReactElementM ViewEventHandler ()
nodeExpression_ ref nl expr mayS = div_ (
    [ "className" $= Style.prefixFromList ["node__expression", "noselect"]
    , "key"       $= "nodeExpression" ]
    ++ handlers) nameElement where
        regularHandlersAndElem = ( [onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nl)]
                                 , elemString $ convert expr )
        (handlers, nameElement) = flip (maybe regularHandlersAndElem) mayS $ \s -> case s ^. Searcher.mode of
            Searcher.Node snl _ _ -> if snl /= nl then regularHandlersAndElem else ([], searcher_ ref s)
            _                     -> regularHandlersAndElem


node_ :: Ref App -> ExpressionNode -> Maybe Searcher -> ReactElementM ViewEventHandler ()
node_ ref model s = React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model, s) mempty

node :: ReactView (Ref App, ExpressionNode, Maybe Searcher)
node = React.defineView name $ \(ref, n, maySearcher) -> case n ^. Node.mode of
    Node.Expanded (Node.Function fs) -> nodeContainer_ ref maySearcher $ Map.elems fs
    _ -> do
        let nodeId          = n ^. Node.nodeId
            nodeLoc         = n ^. Node.nodeLoc
            nodeLimit       = 10000::Int
            zIndex          = n ^. Node.zPos
            z               = if isCollapsed n then zIndex else zIndex + nodeLimit
            isVisualization = Prop.fromNode n ^. Prop.visualizationsEnabled
            hasSelf         = any (\p -> (Port.isSelf $ p ^. Port.portId) && (not $ Port.isInvisible p)) $ Node.inPortsList n
        div_
            [ "key"       $= (nodePrefix <> fromString (show nodeId))
            , "id"        $= (nodePrefix <> fromString (show nodeId))
            , "className" $= Style.prefixFromList ( [ "node", "noselect", (if isCollapsed n then "node--collapsed" else "node--expanded") ]
                                                                       ++ (if returnsError n then ["node--error"] else [])
                                                                       ++ (if n ^. Node.isSelected then ["node--selected"] else [])
                                                                       ++ (if hasSelf then ["node--has-self"] else ["node--no-self"]))
            , "style"     @= Aeson.object [ "zIndex" Aeson..= show z ]
            , onMouseDown   $ handleMouseDown ref nodeLoc
            , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeLoc
            , onDoubleClick $ \e _ -> stopPropagation e : (dispatch ref $ UI.NodeEvent $ Node.Enter nodeLoc)
            ] $ do
            div_
                [ "className" $= Style.prefixFromList [ "node-translate","node__text", "noselect" ]
                , "key"       $= "nodeText"
                ] $ do
                nodeName_ ref nodeLoc (n ^. Node.name) maySearcher
                nodeExpression_ ref nodeLoc (n ^. Node.expression) maySearcher
            nodeBody_ ref n
            div_
                [ "key"       $= "results"
                , "className" $= Style.prefixFromList ["node__results", "node-translate"]
                ] $ do
                nodeShortValue_ n
                if isVisualization then nodeVisualizations_ ref n else return ()
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
nodeBody = React.defineView objNameBody $ \(ref, n) -> do
    let nodeLoc = n ^. Node.nodeLoc
    div_
        [ "key"       $= "nodeBody"
        , "className" $= Style.prefixFromList [ "node__body", "node-translate" ]
        ] $ do
        errorMark_
        selectionMark_
        case n ^. Node.mode of
            Node.Expanded Node.Controls -> nodeProperties_ ref $ Prop.fromNode n
            Node.Expanded Node.Editor   -> multilineField_ [] "editor"
                $ Field.mk ref (fromMaybe def $ n ^. Node.code)
                & Field.onCancel .~ Just (UI.NodeEvent . Node.SetExpression nodeLoc)
            _                           -> ""

nodePorts_ :: Ref App -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodePorts_ ref model = React.viewWithSKey nodePorts objNamePorts (ref, model) mempty

nodePorts :: ReactView (Ref App, ExpressionNode)
nodePorts = React.defineView objNamePorts $ \(ref, n) -> do
    let nodeId             = n ^. Node.nodeId
        nodeLoc            = n ^. Node.nodeLoc
        nodePorts'         = Node.portsList n
        visibleSelfPresent = any (\p -> (Port.isSelf $ p ^. Port.portId) && (not $ Port.isInvisible p)) $ Node.inPortsList n
        ports p =
            forM_ p $ \port -> port_ ref
                                     nodeLoc
                                     port
                                     (if isInPort $ port ^. Port.portId then countArgPorts n else countOutPorts n)
                                     (withOut isOutAll (port ^. Port.portId) && countArgPorts n + countOutPorts n == 1)
                                     (case (n ^. Node.inPorts) of
                                         LabeledTree _ a -> case (a ^. Port.state) of
                                             Port.Connected -> True
                                             _              -> False )
                                     visibleSelfPresent
    svg_
        [ "key"       $= "nodePorts"
        , "className" $= Style.prefixFromList [ "node__ports" ]
        ] $ do
        defs_
            [ "key" $= "defs" ] $ do
            clipPath_
                [ "id"  $= fromString ("port-io-shape-mask-" <> show nodeId)
                , "key" $= "portIoShapeMask"
                ] $
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
                ports $ filter (\port -> (port ^. Port.portId) /= InPortId' [Self]) nodePorts'
                ports $ filter (\port -> (port ^. Port.portId) == InPortId' [Self]) nodePorts'
            else do
                ports $ filter (\port -> (port ^. Port.portId) == InPortId' [Self]) nodePorts'
                forM_  (filter (\port -> (port ^. Port.portId) /= InPortId' [Self]) nodePorts') $ \port -> portExpanded_ ref nodeLoc port
            portPhantom_ ref $ toAnyPortRef nodeLoc $ InPortId' [Arg $ countArgPorts n]

nodeContainer_ :: Ref App -> Maybe Searcher -> [Subgraph] -> ReactElementM ViewEventHandler ()
nodeContainer_ ref maySearcher subgraphs = React.viewWithSKey nodeContainer "node-container" (ref, maySearcher, subgraphs) mempty

nodeContainer :: ReactView (Ref App, Maybe Searcher, [Subgraph])
nodeContainer = React.defineView name $ \(ref, maySearcher, subgraphs) -> do
    div_
        [ "className" $= Style.prefix "subgraphs"
        ] $ forM_ subgraphs $ \subgraph -> do
        let nodes        = subgraph ^. Node.expressionNodes . to HashMap.elems
            lookupNode m = ( m ^. MonadPath.monadType
                           , m ^. MonadPath.path . to (mapMaybe $ flip HashMap.lookup $ subgraph ^. Node.expressionNodes))
            monads       = map lookupNode $ subgraph ^. Node.monads
        div_
            [ "className" $= Style.prefix "subgraph"
            ] $ do
            forM_ nodes $ \n -> node_ ref n (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maySearcher)
            svgPlanes_ $ planeMonads_ $ monads_ monads

filterOutSearcherIfNotRelated :: NodeLoc -> Maybe Searcher -> Maybe Searcher
filterOutSearcherIfNotRelated _  Nothing  = Nothing
filterOutSearcherIfNotRelated nl (Just s) = if Searcher.isSearcherRelated nl s then return s else Nothing
