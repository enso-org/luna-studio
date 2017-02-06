{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node where

import qualified Data.Aeson                             as Aeson
import qualified Data.Map.Lazy                          as Map
import qualified Data.Text                              as Text
import           Empire.API.Data.Node                   (NodeId)
import           Empire.API.Data.Port                   (InPort (..), PortId (..))
import           Luna.Studio.Action.Geometry            (countSameTypePorts, isPortSingle)
import           Luna.Studio.Data.Matrix                (transformTranslateToSvg)
import qualified Luna.Studio.Event.Mouse                as Mouse
import qualified Luna.Studio.Event.UI                   as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node           as Node
import           Luna.Studio.React.Model.App            (App)
import           Luna.Studio.React.Model.Node           (Node)
import qualified Luna.Studio.React.Model.Node           as Node
import qualified Luna.Studio.React.Model.NodeProperties as Properties
import qualified Luna.Studio.React.Model.Port           as Port
import           Luna.Studio.React.Store                (Ref, dispatch)
import           Luna.Studio.React.View.CommonElements  (blurBackground_,selectionMark_)
import           Luna.Studio.React.View.NodeProperties  (nodeProperties_)
import           Luna.Studio.React.View.Port            (portExpanded_, port_)
import           Luna.Studio.React.View.Visualization   (visualization_)
import           React.Flux
import qualified React.Flux                             as React


objName :: JSString
objName = "node"

handleMouseDown :: Ref App -> NodeId -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref nodeId e m =
    if Mouse.withoutMods m Mouse.leftButton
    then stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeId)
    else []

node :: ReactView (Ref App, Node)
node = React.defineView objName $ \(ref, n) -> do
    let nodeId    = n ^. Node.nodeId
        pos       = n ^. Node.position
        nodePorts = Map.elems $ n ^. Node.ports
        ports p   = forM_ p $ \port -> port_ ref port (countSameTypePorts port p) (isPortSingle port p)
        nodeLimit = 10000::Int
        zIndex    = n ^. Node.zPos
        z         = if n ^. Node.isExpanded then zIndex + nodeLimit else zIndex
    div_
        [ "key"       $= fromString (show nodeId)
        , "className" $= "luna-node-root luna-noselect"
        , "style"     @= Aeson.object [ "zIndex" Aeson..= show z ]
        ] $ do
        div_
            [ "key"       $= "nodeTrans"
            , "className" $= "luna-node-trans"
            ] $
            div_
                [ "key"         $= "nodeBodyRoot"
                , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
                , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
                , onMouseDown   $ handleMouseDown ref nodeId
                , "className"   $= (fromString $ "luna-node" <> (if n ^. Node.isExpanded then " luna-node--expanded" else " luna-node--collapsed")
                                                             <> (if n ^. Node.isSelected then " luna-node--selected" else []))
                , "style"       @= Aeson.object
                    [ "transform" Aeson..= transformTranslateToSvg pos
                    ]
                ] $ do
                div_
                    [ "key"       $= "main"
                    , "className" $= "luna-node__main"
                    ] $ do
                    selectionMark_
                    div_
                        [ "key"       $= "properties-crop"
                        , "className" $= "luna-node__properties-crop"
                        , "id"        $= ("node-" <> fromString (show nodeId))
                        ] $ do
                        blurBackground_
                        if n ^. Node.isExpanded then (nodeProperties_ ref $ Properties.fromNode n) else ""
                div_
                    [ "key"       $= "visualization"
                    , "className" $= "luna-node__visuals"
                    ] $ forM_ (n ^. Node.value) $ visualization_ ref nodeId def
                svg_
                    [ "key"       $= "essentials"
                    , "className" $= "luna-node__essentials"
                    ] $ do
                    if  n ^. Node.isExpanded then do
                        ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
                        forM_  (filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts) (\port -> portExpanded_ ref port)
                    else do
                        ports $ filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts
                        ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
        div_
            [ "key"       $= "nameTrans"
            , "className" $= "luna-name-trans"
            ] $
            div_
                [ "key"         $= "nameRoot"
                , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
                , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
                , onMouseDown   $ handleMouseDown ref nodeId
                , "style"       @= Aeson.object [ "transform" Aeson..= transformTranslateToSvg pos ]
                , "className"   $= (fromString $ "luna-node" <> (if n ^. Node.isExpanded then " luna-node--expanded" else " luna-node--collapsed")
                                                             <> (if n ^. Node.isSelected then " luna-node--selected" else []))
                ] $ do
                svg_
                    [ "key" $= "name" ] $ do
                    text_
                        [ "key"         $= "nameText"
                        , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeId)
                        , "className"   $= "luna-node__name"
                        , "y"           $= "-36"
                        ] $ elemString $ Text.unpack $ n ^. Node.expression

node_ :: Ref App -> Node -> ReactElementM ViewEventHandler ()
node_ ref model = React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model) mempty
