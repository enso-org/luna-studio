{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node where

import qualified Data.Aeson                             as Aeson
import qualified Data.Map.Lazy                          as Map
import qualified Data.Text                              as Text
import           Data.Vector                            (x, y)
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
import           Luna.Studio.React.View.NodeProperties  (nodeProperties_)
import           Luna.Studio.React.View.Port            (portExpanded_, port_)
import           Luna.Studio.React.View.Visualization   (visualization_)
import           React.Flux
import qualified React.Flux                             as React


objName :: JSString
objName = "node"

handleMouseDown :: Ref App -> NodeId -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref nodeId e m =
    if (Mouse.withoutMods m Mouse.leftButton)
    then stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeId)
    else []

node :: ReactView (Ref App, Node)
node = React.defineView objName $ \(ref, n) -> do
    let nodeId    = n ^. Node.nodeId
        pos       = n ^. Node.position
        nodePorts = Map.elems $ n ^. Node.ports
        ports p   = forM_ p $ \port -> port_ ref port (countSameTypePorts port p) (isPortSingle port p)
        offsetX   = show $ pos ^. x
        offsetY   = show $ pos ^. y
        nodeLimit = 10000::Int
        zIndex    = 1::Int -- FIXME – set order of nodes
        z         = if n ^. Node.isExpanded then zIndex + nodeLimit else zIndex
    div_
        [ "key"       $= fromString (show nodeId)
        , "className" $= "node-root noselect"
        , "style"     @= Aeson.object [ "zIndex" Aeson..= (show z) ]
        ] $ do
        div_
            [ "key"       $= "nodeTrans"
            , "className" $= "node-trans"
            ] $ do
            div_
                [ "key"         $= "nodeBodyRoot"
                , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
                , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
                , onMouseDown   $ handleMouseDown ref nodeId
                , "className"   $= (fromString $ "node" <> (if n ^. Node.isExpanded then " node--expanded" else " node--collapsed")
                                                        <> (if n ^. Node.isSelected then " node--selected" else []))
                , "style"       @= Aeson.object
                    [ "transform" Aeson..= (transformTranslateToSvg offsetX offsetY)
                    ]
                ] $ do
                --svg_
                --    [ "className" $= "node__selection-mark"
                --    , "key"       $= "selection-mark"
                --    ] $ rect_ def mempty
                nodeProperties_ ref $ Properties.fromNode n
                div_
                    [ "key"       $= "visualization"
                    , "className" $= "node__visuals"
                    ] $ forM_ (n ^. Node.value) visualization_
                svg_
                    [ "key"       $= "essentials"
                    , "className" $= "node__essentials"
                    ] $ do
                    if  n ^. Node.isExpanded then do
                        ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
                        forM_  (filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts) (\port -> portExpanded_ ref port)
                    else do
                        ports $ filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts
                        ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
        div_
            [ "key"       $= "nameTrans"
            , "className" $= "name-trans"
            ] $ do
            div_
                [ "key"         $= "nameRoot"
                , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
                , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
                , onMouseDown   $ handleMouseDown ref nodeId
                , "style"       @= Aeson.object [ "transform" Aeson..= (transformTranslateToSvg offsetX offsetY) ]
                , "className"   $= (fromString $ "node" <> (if n ^. Node.isExpanded then " node--expanded" else " node--collapsed")
                                                        <> (if n ^. Node.isSelected then " node--selected" else []))
                ] $ do
                svg_
                    [ "key" $= "name" ] $ do
                    text_
                        [ "key"         $= "nameText"
                        , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeId)
                        , "className"   $= "node__name"
                        , "y"           $= "-36"
                        ] $ elemString $ Text.unpack $ n ^. Node.expression

node_ :: Ref App -> Node -> ReactElementM ViewEventHandler ()
node_ ref model = React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model) mempty
