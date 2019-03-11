{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode where

import Common.Prelude

import qualified Data.Aeson                                           as Aeson
import qualified Data.HashMap.Strict                                  as HashMap
import qualified Data.Map.Lazy                                        as Map
import qualified Data.Set                                             as Set
import qualified Data.Text                                            as Text
import qualified JS.Mount                                             as Mount
import qualified JS.UI                                                as UI
import qualified LunaStudio.Data.MonadPath                            as MonadPath
import qualified LunaStudio.Data.PortRef                              as PortRef
import qualified NodeEditor.Event.Mouse                               as Mouse
import qualified NodeEditor.Event.UI                                  as UI
import qualified NodeEditor.React.Event.Node                          as Node
import qualified NodeEditor.React.Event.Visualization                 as Visualization
import qualified NodeEditor.React.Model.Field                         as Field
import qualified NodeEditor.React.Model.Node.ExpressionNode           as Node
import qualified NodeEditor.React.Model.Node.ExpressionNodeProperties as Prop
import qualified NodeEditor.React.Model.Port                          as Port
import qualified NodeEditor.React.Model.Searcher                      as Searcher
import qualified NodeEditor.React.Model.Searcher.Mode                 as SearcherMode
import qualified NodeEditor.React.Model.Searcher.Mode.Node            as NodeSearcher
import qualified NodeEditor.React.Model.Visualization                 as Vis
import qualified NodeEditor.React.View.Style                          as Style
import qualified React.Flux                                           as React

import Control.Arrow                                   ((&&&))
import Data.Matrix                                     (Matrix)
import Data.Set                                        (Set)
import JS.Name                                         (toName)
import LunaStudio.Data.Constants                       (gridSize)
import LunaStudio.Data.Matrix                          (showNodeMatrix,
                                                        showNodeTranslate)
import LunaStudio.Data.NodeLoc                         (toNodeIdList)
import LunaStudio.Data.PortRef                         (InPortRef)
import NodeEditor.React.IsRef                          (IsRef, dispatch)
import NodeEditor.React.Model.Constants                (expandedNodePadding,
                                                        nodeRadius,
                                                        selectionPadding)
import NodeEditor.React.Model.Node.ExpressionNode      (ExpressionNode, NodeLoc,
                                                        Subgraph,
                                                        argumentConstructorRef,
                                                        countVisibleArgPorts,
                                                        countVisibleInPorts,
                                                        countVisibleOutPorts,
                                                        isAnyPortHighlighted,
                                                        isCollapsed,
                                                        returnsError,
                                                        visibleArgPortNumber,
                                                        visibleInPortNumber,
                                                        visibleOutPortNumber)
import NodeEditor.React.Model.Port                     (isAll, isInPort,
                                                        withOut)
import NodeEditor.React.View.ColorizedExpression       (colorizedExpression_)
import NodeEditor.React.View.ExpressionNode.NodeValue  (nodeValue_)
import NodeEditor.React.View.ExpressionNode.Properties (nodeProperties_)
import NodeEditor.React.View.Field                     (multilineField_)
import NodeEditor.React.View.Monad                     (monads_)
import NodeEditor.React.View.Plane                     (planeMonads_)
import NodeEditor.React.View.Port                      (argumentConstructor_,
                                                        portExpanded_, port_)
import NodeEditor.React.View.Searcher                  (searcher_)
import NodeEditor.React.View.Style                     (errorMark_,
                                                        selectionMark_)
import React.Flux


name, objNameBody, objNamePorts, objNameDynStyles :: JSString
name             = "node"
objNameBody      = "node-body"
objNamePorts     = "node-ports"
objNameDynStyles = "node-dynstyle"

prefixNode :: JSString -> JSString
prefixNode = Mount.prefix . ("node-" <>)

nameLabelId :: JSString
nameLabelId = Mount.prefix "focus-nameLabel"

focusNameLabel :: IO ()
focusNameLabel = UI.focus nameLabelId

handleMouseDown :: IsRef ref => ref -> NodeLoc -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref nodeLoc e m =
    if Mouse.withoutMods m Mouse.leftButton || Mouse.withShift m Mouse.leftButton
    then stopPropagation e : dispatch ref (UI.NodeEvent $ Node.Event nodeLoc $ Node.MouseDown m)
    else []

nodeName_ :: IsRef ref => ref -> NodeLoc -> Maybe Text -> Bool -> Maybe Searcher.Properties -> Bool -> ReactElementM ViewEventHandler ()
nodeName_ ref nl name' visVisible mayS blockVisInBc = React.viewWithSKey nodeName  "node-name" (ref, nl, name', visVisible, mayS, blockVisInBc) mempty

nodeName :: IsRef ref => ReactView (ref, NodeLoc, Maybe Text, Bool, Maybe Searcher.Properties, Bool)
nodeName = React.defineView "node-name" $ \(ref, nl, name', visualizationVisible, mayS, blockVisInBc) -> do
    let regularHandlersAndElem = ( [onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.Event nl Node.EditName)]
                                 , elemString . convert $ fromMaybe def name' )
        (handlers, nameElement) = flip (maybe regularHandlersAndElem) mayS $ \s 
            -> case s ^. Searcher.searcher . Searcher.mode of
                SearcherMode.Node ns -> case ns ^. NodeSearcher.mode of
                    NodeSearcher.NodeNameMode {} 
                        -> if (ns ^. NodeSearcher.nodeLoc) /= nl 
                            then regularHandlersAndElem 
                            else ([], searcher_ ref s)
                    _ -> regularHandlersAndElem
                _     -> regularHandlersAndElem
    div_
        ([ "className" $= Style.prefixFromList ["node__name", "noselect"]
        , "key" $= "nodeName"
        ] <> handlers) $ do
        div_
            [ "className" $= Style.prefix "node__name--positioner"
            ] $ do
            nameElement
            unless (blockVisInBc) $ svg_
                [ "key"       $= "ctrlSwitch"
                , "className" $= Style.prefix "ctrl-icon"
                , "xmlns"     $= "http://www.w3.org/2000/svg"
                , "viewBox"   $= "0 0 24 24"
                , onDoubleClick $ \e _ -> [stopPropagation e]
                , onClick       $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ Visualization.Event (Vis.Node nl) Visualization.ToggleVisualizations
                ] $ if visualizationVisible
                    then path_ [ "d"         $= Style.iconEye
                               , "className" $= Style.prefix "icon--on"
                               ] mempty
                    else path_ [ "d"         $= Style.iconEyeDisabled
                               , "className" $= Style.prefixFromList ["icon--off"]
                               ] mempty


nodeExpression_ :: IsRef ref => ref -> NodeLoc -> Text -> Maybe Searcher.Properties -> ReactElementM ViewEventHandler ()
nodeExpression_ ref nl expr mayS = React.viewWithSKey nodeExpression "node-expression" (ref, nl, expr, mayS) mempty

nodeExpression :: IsRef ref => ReactView (ref, NodeLoc, Text, Maybe Searcher.Properties)
nodeExpression = React.defineView "node-expression" $ \(ref, nl, expr, mayS) -> do
    let isLong = Text.length expr > 64

        regularHandlersAndElem  = ( [onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.Event nl Node.EditExpression)]
                                  , colorizedExpression_ expr )
        (handlers, nameElement) = flip (maybe regularHandlersAndElem) mayS $ \s
            -> case s ^. Searcher.searcher . Searcher.mode of
                SearcherMode.Node ns -> case ns ^. NodeSearcher.mode of
                    NodeSearcher.ExpressionMode {}
                        -> if (ns ^. NodeSearcher.nodeLoc) /= nl
                            then regularHandlersAndElem
                            else ([], searcher_ ref s)
                    _ -> regularHandlersAndElem
                _     -> regularHandlersAndElem
    div_
        (
        [ "className" $= Style.prefixFromList (["node__expression", "noselect"] <> (if isLong then ["node__expression--long"] else []))
        , "key"       $= "nodeExpression" ] <> handlers
        ) nameElement

node_ :: IsRef ref => ref -> ExpressionNode -> Bool -> Bool -> Maybe Searcher.Properties -> Maybe InPortRef -> Set NodeLoc -> Bool -> ReactElementM ViewEventHandler ()
node_ ref model isTopLevel performConnect s mayEditedTextPortControlPortRef relatedNodesWithVis blockVisInBc =
    React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model, isTopLevel, performConnect, s, mayEditedTextPortControlPortRef, relatedNodesWithVis, blockVisInBc) mempty

node :: IsRef ref => ReactView (ref, ExpressionNode, Bool, Bool, Maybe Searcher.Properties, Maybe InPortRef, Set NodeLoc, Bool)
node = React.defineView name $ \(ref, n, isTopLevel, performConnect, maySearcher, mayEditedTextPortControlPortRef, relatedNodesWithVis, blockVisInBc) -> case n ^. Node.mode of
    Node.Expanded (Node.Function fs) -> nodeContainer_ ref isTopLevel performConnect maySearcher mayEditedTextPortControlPortRef relatedNodesWithVis (Map.elems fs) blockVisInBc
    _ -> do
        let nodeId        = n ^. Node.nodeId
            nodeLoc       = n ^. Node.nodeLoc
            isDef         = n ^. Node.isDefinition
            nodeLimit     = 10000::Int
            zIndex        = n ^. Node.zPos
            z             = zIndex + 11::Int --if isCollapsed n then zIndex else zIndex + nodeLimit
            hasSelf       = any (\p -> (Port.isSelf $ p ^. Port.portId) && (not $ Port.isInvisible p)) $ Node.inPortsList n
            hasAlias      = any (Port.isAlias . (^. Port.portId)) $ Node.inPortsList n
            mayVisVisible = if blockVisInBc then def else const (n ^. Node.visualizationsEnabled) <$> n ^. Node.defaultVisualizer
            showValue     = not $ n ^. Node.visualizationsEnabled && Set.member nodeLoc relatedNodesWithVis
            expression    = n ^. Node.expression
            highlight     = if n ^. Node.isMouseOver && (not performConnect || not (isAnyPortHighlighted n)) then ["hover"] else []
                        --  && (n ^. Node.argConstructorMode /= Port.Highlighted)
                        --  && (not $ any Port.isHighlighted (inPortsList n))
                        --  && (not $ any Port.isHighlighted (outPortsList n)) then ["hover"] else []
            needAdjustment = countVisibleOutPorts n <= countVisibleInPorts n || countVisibleInPorts n == 0
            hasArgConstructor = not isTopLevel && elem (n ^. Node.argConstructorMode) [Port.Normal, Port.Highlighted]
        div_
            [ "key"       $= prefixNode (jsShow nodeId)
            , "id"        $= prefixNode (jsShow nodeId)
            , "className" $= Style.prefixFromList
                            ( [ "node"
                              , "noselect"
                              , convert $ "node-name" <> maybe "" (\a -> "-" <> toName a) (n ^. Node.name)
                              , (if isCollapsed n                               then "node--collapsed"                    else "node--expanded") ]
                             <> (if returnsError n                              then ["node--error"]                      else [])
                             <> (if n ^. Node.isSelected                        then ["node--selected"]                   else [])
                             <> (if n ^. Node.isMouseOver && not performConnect then ["show-ctrl-icon"]                   else [])
                             <> (if hasSelf                                     then ["node--has-self"]                   else ["node--no-self"])
                             <> (if hasAlias                                    then ["node--has-alias"]                  else ["node--no-alias"])
                             <> (if hasArgConstructor                           then ["node--has-arg-constructor"]        else [])
                             <> (if needAdjustment                              then ["node--arg-constructor-adjustment"] else [])
                             <> highlight
                            )
            , "style"     @= Aeson.object [ "zIndex" Aeson..= show z ]
            , onMouseDown   $ handleMouseDown ref nodeLoc
            , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Event nodeLoc $ Node.Select m
            , onDoubleClick $ \e _ -> stopPropagation e : (dispatch ref $ UI.NodeEvent $ Node.Event nodeLoc Node.Enter)
            , onMouseEnter  $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Event nodeLoc Node.MouseEnter
            , onMouseLeave  $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Event nodeLoc Node.MouseLeave
            ] $ do
            div_
                [ "className" $= Style.prefixFromList [ "node-translate","node__text", "noselect" ]
                , "key"       $= "nodeText"
                ] $ do
                nodeName_ ref nodeLoc (n ^. Node.name) (n ^. Node.visualizationsEnabled) maySearcher blockVisInBc
                unless (isTopLevel && isDef) $ nodeExpression_ ref nodeLoc expression maySearcher
            nodeBody_ ref n mayEditedTextPortControlPortRef
            when showValue $ nodeValue_ ref n
            nodePorts_ ref n hasAlias hasSelf isTopLevel

nodeDynamicStyles_ :: Matrix Double -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeDynamicStyles_ camera n = React.viewWithSKey nodeDynamicStyles (jsShow $ n ^. Node.nodeId) (camera, n) mempty

nodeDynamicStyles :: ReactView (Matrix Double, ExpressionNode)
nodeDynamicStyles = React.defineView objNameDynStyles $ \(camera, n) -> style_ $ do
    let nodeId  = n ^. Node.nodeId
        nodePos = n ^. Node.position
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " .luna-node-translate--name { transform: " <> showNodeTranslate camera nodePos <> " }"
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " .luna-node-translate { transform: "       <> showNodeTranslate camera nodePos <> " }"
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " .luna-node-transform { transform: "       <> showNodeMatrix    camera nodePos <> " }"
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " path.luna-port__shape { clip-path: url(#port-io-shape-mask-"   <> show nodeId <> ") }"
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " path.luna-port__select { clip-path: url(#port-io-select-mask-" <> show nodeId <> ") }"

nodeBody_ :: IsRef ref => ref -> ExpressionNode -> Maybe InPortRef -> ReactElementM ViewEventHandler ()
nodeBody_ ref model mayEditedTextPortControlPortRef = React.viewWithSKey nodeBody "node-body" (ref, model, mayEditedTextPortControlPortRef) mempty

nodeBody :: IsRef ref => ReactView (ref, ExpressionNode, Maybe InPortRef)
nodeBody = React.defineView objNameBody $ \(ref, n, mayEditedTextPortControlPortRef) -> do
    let nodeLoc = n ^. Node.nodeLoc
    div_
        [ "key"       $= "nodeBody"
        , "id"        $= prefixNode ("body-" <> jsShow (n ^. Node.nodeId))
        , "className" $= Style.prefixFromList [ "node__body"
                                              , "node-translate"
                                              ]
        ] $ do
        errorMark_
        case n ^. Node.mode of
            Node.Expanded Node.Controls -> nodeProperties_ ref (Prop.fromNode n) mayEditedTextPortControlPortRef $ max (countVisibleInPorts n) $ countVisibleOutPorts n
            Node.Expanded Node.Editor   -> multilineField_ [] "editor"
                $ Field.mk ref (n ^. Node.code)
                & Field.onCancel .~ Just (UI.NodeEvent . Node.Event nodeLoc . Node.SetExpression)
            _                           -> ""

nodePorts_ :: IsRef ref => ref -> ExpressionNode -> Bool -> Bool -> Bool -> ReactElementM ViewEventHandler ()
nodePorts_ ref model hasAlias hasSelf isTopLevel = React.viewWithSKey nodePorts objNamePorts (ref, model, hasAlias, hasSelf, isTopLevel) mempty

nodePorts :: IsRef ref => ReactView (ref, ExpressionNode, Bool, Bool, Bool)
nodePorts = React.defineView objNamePorts $ \(ref, n, hasAlias, hasSelf, isTopLevel) -> do
    let nodeId       = n ^. Node.nodeId
        nodeLoc      = n ^. Node.nodeLoc
        argPortNum p = visibleArgPortNumber n $ p ^. Port.portId
        inPortNum  p = visibleInPortNumber  n $ p ^. Port.portId
        outPortNum p = visibleOutPortNumber n $ p ^. Port.portId
        argPorts     = map (convert &&& argPortNum) $ Node.inPortsList n
        inPorts      = map (convert &&& inPortNum)  $ Node.inPortsList n
        outPorts     = map (convert &&& outPortNum) $ Node.outPortsList n
        nodePorts'   = Node.portsList n
        ports p      = forM_ p $ \(port, num) -> port_ ref
                                                       nodeLoc
                                                       port
                                                       num
                                                       (if isInPort $ port ^. Port.portId then countVisibleArgPorts n else countVisibleOutPorts n)
                                                       (withOut isAll (port ^. Port.portId) && countVisibleArgPorts n + countVisibleOutPorts n == 1)
                                                       isTopLevel
    svg_
        [ "viewBox"   $= ("-" <> jsShow nodeRadius <> " -" <> jsShow nodeRadius <> " " <> (jsShow $ nodeRadius * 2) <> " " <> (jsShow $ nodeRadius * 2))
        , "key"       $= "nodePorts"
        , "className" $= Style.prefix "node__ports"
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
            [ "key"       $= "nodeTransform"
            , "className" $= Style.prefix "node-transform"
            ] $ do
            if isCollapsed n then do
                selectionMark_ $ (nodeRadius + if n ^. Node.isSelected then selectionPadding else 0) * 2
                ports argPorts
                ports outPorts
            else do
                selectionMark_ $ (fromIntegral $ max (countVisibleInPorts n) $ countVisibleOutPorts n) * gridSize + 2 * expandedNodePadding + if n ^. Node.isSelected then 2 * selectionPadding else 0
                forM_ inPorts
                 $ uncurry (portExpanded_ ref nodeLoc)
                forM_ outPorts $ uncurry (portExpanded_ ref nodeLoc)

            argumentConstructor_ ref (argumentConstructorRef n) (countVisibleInPorts n) (n ^. Node.argConstructorMode == Port.Highlighted) hasAlias hasSelf

nodeContainer_ :: IsRef ref => ref -> Bool -> Bool -> Maybe Searcher.Properties -> Maybe InPortRef -> Set NodeLoc -> [Subgraph] -> Bool -> ReactElementM ViewEventHandler ()
nodeContainer_ ref isTopLevel performConnect maySearcher mayEditedTextPortControlPortRef nodesWithVis subgraphs blockVisInBc =
    React.viewWithSKey nodeContainer "node-container" (ref, isTopLevel, performConnect, maySearcher, mayEditedTextPortControlPortRef, nodesWithVis, subgraphs, blockVisInBc) mempty

nodeContainer :: IsRef ref => ReactView (ref, Bool, Bool, Maybe Searcher.Properties, Maybe InPortRef, Set NodeLoc, [Subgraph], Bool)
nodeContainer = React.defineView name $ \(ref, isTopLevel, performConnect, maySearcher, mayEditedTextPortControlPortRef, nodesWithVis, subgraphs, blockVisInBc) -> do
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
            forM_ nodes $ \n -> node_ ref
                                      n
                                      isTopLevel
                                      performConnect
                                      (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maySearcher)
                                      (filterOutEditedTextControlIfNotRelated (n ^. Node.nodeLoc) mayEditedTextPortControlPortRef)
                                      (Set.filter (Node.containsNode (n ^. Node.nodeLoc)) nodesWithVis)
                                      blockVisInBc
            planeMonads_ $ monads_ monads

filterOutSearcherIfNotRelated :: NodeLoc -> Maybe Searcher.Properties
    -> Maybe Searcher.Properties
filterOutSearcherIfNotRelated _  Nothing  = Nothing
filterOutSearcherIfNotRelated nl (Just s) = if Searcher.isRelated nl s
    then pure s
    else Nothing

filterOutEditedTextControlIfNotRelated :: NodeLoc -> Maybe InPortRef
    -> Maybe InPortRef
filterOutEditedTextControlIfNotRelated _  Nothing = Nothing
filterOutEditedTextControlIfNotRelated nl (Just portRef)
    = if isPrefixOf (toNodeIdList nl) (toNodeIdList $ portRef ^. PortRef.nodeLoc)
        then Just portRef
        else Nothing
