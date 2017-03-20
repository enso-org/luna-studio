{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node where

import qualified Data.Aeson                             as Aeson
import qualified Data.Map.Lazy                          as Map
import           Data.Matrix                            as Matrix
import           Data.Matrix                            (Matrix)
import           Data.Position                          (Position (Position), x, y)
import           Data.Vector                            (Vector2 (Vector2))
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResult
import qualified JS.Config                              as Config
import           Luna.Studio.Data.Matrix                (translatePropertyValue2)
import qualified Luna.Studio.Event.Mouse                as Mouse
import qualified Luna.Studio.Event.UI                   as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node           as Node
import           Luna.Studio.React.Model.App            (App)
import qualified Luna.Studio.React.Model.Field          as Field
import           Luna.Studio.React.Model.Node           (Node, NodeId, NodeType (ExpressionNode), Subgraph, countArgPorts, countOutPorts,
                                                         isCollapsed)
import qualified Luna.Studio.React.Model.Node           as Node
import qualified Luna.Studio.React.Model.NodeProperties as Properties
import           Luna.Studio.React.Model.Port           (InPort (Self), PortId (InPortId), isAll, isInPort)
import qualified Luna.Studio.React.Model.Port           as Port
import           Luna.Studio.React.Store                (Ref, dispatch)
import           Luna.Studio.React.View.Field           (multilineField_)
import           Luna.Studio.React.View.Node.Properties (nodeProperties_)
import           Luna.Studio.React.View.Port            (portExpanded_, port_)
import           Luna.Studio.React.View.Style           (blurBackground_, selectionMark_)
import qualified Luna.Studio.React.View.Style           as Style
import           Luna.Studio.React.View.Visualization   (visualization_)
import           React.Flux
import qualified React.Flux                             as React


name :: JSString
name = "node"

nodePrefix :: JSString
nodePrefix = Config.prefix "node-"

handleMouseDown :: Ref App -> NodeId -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref nodeId e m =
    if Mouse.withoutMods m Mouse.leftButton || Mouse.withShift m Mouse.leftButton
    then stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeId)
    else []

node :: ReactView (Ref App, Node)
node = React.defineView name $ \(ref, n) -> do
    let nodeId    = n ^. Node.nodeId
        nodeLimit = 10000::Int
        zIndex    = n ^. Node.zPos
        z         = if isCollapsed n then zIndex else zIndex + nodeLimit
    div_
        [ "key"       $= (nodePrefix <> fromString (show nodeId))
        , "id"        $= (nodePrefix <> fromString (show nodeId))
        , "className" $= Style.prefix "node-root"
        , "style"     @= Aeson.object [ "zIndex" Aeson..= show z ]
        , onMouseDown   $ handleMouseDown ref nodeId
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $ do
        div_
            [ "key"       $= "nodeTrans"
            , "className" $= Style.prefix "node-trans"
            ] $
            nodeBody_ ref n
        div_
            [ "key"       $= "nameTrans"
            , "className" $= Style.prefix "name-trans"
            ] $
            div_
                [ "key"         $= "nameRoot"
                , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
                , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
                , "className"   $= Style.prefixFromList ( [ "node" , (if isCollapsed n then "node--collapsed"   else "node--expanded") ]
                                                          ++ (if n ^. Node.isSelected  then ["node--selected"]  else []) )
                ] $
                svg_
                    [ "key" $= "name" ] $ do
                    text_
                        [ "key"         $= "expressionText"
                        , "y"           $= "-16"
                        , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeId)
                        , "className"   $= Style.prefixFromList [ "node__name", "node__name--expression", "noselect" ]
                        ] $ elemString $ case n ^. Node.nodeType of
                                (ExpressionNode expr) -> convert $ expr
                                _                     -> "" -- TODO[PM, JK, LJK]: Find out what to do with other types
                    text_
                        [ "key"         $= "nameText"
                        , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeId)
                        , "className"   $= Style.prefixFromList [ "node__name", "noselect" ]
                        ] $ elemString $ convert $ Properties.fromNode n ^. Properties.name

node_ :: Ref App -> Node -> ReactElementM ViewEventHandler ()
node_ ref model = React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model) mempty

nodeDynamicStyles_ :: Matrix Double -> Node -> ReactElementM ViewEventHandler ()
nodeDynamicStyles_ camera n = do
    let nodeId = n ^. Node.nodeId
        pos    = expressionPosition camera (n ^. Node.position)
    elemString $ "#" <> Config.mountPoint <> "-node-" <> fromString (show nodeId)
                     <> " .luna-name-trans { transform: translate(" <> show (pos ^. x) <> "px, " <> show (pos ^. y) <> "px) }"

expressionPosition :: Matrix Double -> Position -> Position
expressionPosition camera n = Position (Vector2 x' y')
    where posX  = n ^. x
          posY  = n ^. y - 36
          camX  = (Matrix.toList camera)!!12
          camY  = (Matrix.toList camera)!!13
          scale = (Matrix.toList camera)!!0
          x'    = fromInteger (round $ camX + (scale * posX) :: Integer)
          y'    = fromInteger (round $ camY + (scale * posY) :: Integer)

nodeBody :: ReactView (Ref App, Node)
nodeBody = React.defineView "node-body" $ \(ref, n) -> do
    let nodeId    = n ^. Node.nodeId
        pos       = n ^. Node.position
        nodePorts = Map.elems $ n ^. Node.ports
        ports p   = forM_ p $ \port -> port_ ref
                                             nodeId
                                             port
                                            (if isInPort $ port ^. Port.portId then countArgPorts n else countOutPorts n)
                                            (isAll (port ^. Port.portId) && countArgPorts n + countOutPorts n == 1)
    div_
        [ "key"         $= "nodeBodyRoot"
        , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
        , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
        , "className"   $= Style.prefixFromList ([ "node", if isCollapsed n then "node--collapsed" else "node--expanded" ]
                                                ++ (if n ^. Node.isSelected  then ["node--selected"]  else []))
        , "style"       @= Aeson.object [ "transform" Aeson..= translatePropertyValue2 pos ]
        ] $ do
        div_
            [ "key"       $= "shortValue"
            ] $ mapM_ (elemString . convert) $ n ^? Node.value . _Just .  NodeResult._Value . _1
        div_
            [ "key"       $= "main"
            , "className" $= Style.prefix "node__main"
            ] $ do
            selectionMark_
            div_
                [ "key"       $= "properties-crop"
                , "className" $= Style.prefix "node__properties-crop"
                ] $ do
                blurBackground_
                case n ^. Node.mode of
                    Node.Collapsed                   -> ""
                    Node.Expanded (Node.Function fs) -> nodeContainer_ ref fs
                    Node.Expanded Node.Controls      -> nodeProperties_ ref $ Properties.fromNode n
                    Node.Expanded Node.Editor        -> multilineField_ [] "editor"
                        $ Field.mk ref (fromMaybe def $ n ^. Node.code)
                        & Field.onCancel .~ Just (UI.NodeEvent . Node.SetCode nodeId)

        div_
            [ "key"       $= "visualization"
            , "className" $= Style.prefix "node__visuals"
            ] $ forM_ (n ^. Node.value) $ visualization_ ref nodeId def
        svg_
            [ "key"       $= "essentials"
            , "className" $= Style.prefix "node__essentials"
            ] $ do
            if isCollapsed n then do
                ports $ filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts
                ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
            else do
                ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
                forM_  (filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts) $ \port -> portExpanded_ ref nodeId port

nodeBody_ :: Ref App -> Node -> ReactElementM ViewEventHandler ()
nodeBody_ ref model = React.viewWithSKey nodeBody "node-body" (ref, model) mempty

nodeContainer_ :: Ref App -> [Subgraph] -> ReactElementM ViewEventHandler ()
nodeContainer_ ref sgs = React.viewWithSKey nodeContainer "node-container" (ref, sgs) mempty

nodeContainer :: ReactView (Ref App, [Subgraph])
nodeContainer = React.defineView name $ \(_ref, sgs) -> do
    div_ $ elemString $ show sgs
