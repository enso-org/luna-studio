{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node where

import qualified Data.Aeson                             as Aeson
import qualified Data.Map.Lazy                          as Map
import qualified Data.Text                              as Text
import           Data.Vector                            (x, y)
import           Empire.API.Data.Node                   (NodeId)
import           Empire.API.Data.Port                   (InPort (..), PortId (..))
import           Luna.Studio.Action.Geometry            (countSameTypePorts, isPortSingle)
import qualified Luna.Studio.Data.CameraTransformation  as CameraTransformation
import           Luna.Studio.Data.Matrix                (transformTranslateToSvg)
import           Data.Matrix                            (Matrix,(!))
import qualified Luna.Studio.Event.Mouse                as Mouse
import qualified Luna.Studio.Event.UI                   as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node           as Node
import           Luna.Studio.React.Model.App            (App)
import           Luna.Studio.React.Model.Node           (Node)
import qualified Luna.Studio.React.Model.Node           as Node
import           Luna.Studio.React.Model.NodeEditor     (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import qualified Luna.Studio.React.Model.NodeProperties as Properties
import           Luna.Studio.React.Model.Port           (Port (..))
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

ports :: Ref App -> [Port] -> ReactElementM ViewEventHandler ()
ports nodeRef ports = forM_ ports $ \port -> port_ nodeRef port (countSameTypePorts port ports) (isPortSingle port ports)

portsExpanded :: Ref App -> [Port] -> ReactElementM ViewEventHandler ()
portsExpanded nodeRef ports = forM_ ports $ \port -> portExpanded_ nodeRef port

--TODO inline div and others
node :: ReactView (Ref App, Node, Matrix Double)
node = React.defineView objName $ \(ref, n, camTransform ) -> do
    let nodeId     = n ^. Node.nodeId
        pos        = n ^. Node.position
        nodePorts  = Map.elems $ n ^. Node.ports
        offsetX    = show $ pos ^. x
        offsetY    = show $ pos ^. y
        nodeLimit  = 10000::Int
        zIndex     = 1::Int -- FIXME – set order of nodes
        z          = if n ^. Node.isExpanded then zIndex + nodeLimit else zIndex
        zoomFactor = camTransform ! (1, 1)
        fontSize   = show $ 12/zoomFactor
    div_
        [ "key"         $= fromString (show nodeId)
        , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
        , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
        , onMouseDown   $ handleMouseDown ref nodeId
        , "className"   $= (fromString $ "node noselect " <> (if n ^. Node.isExpanded then " node--expanded" else " node--collapsed")
                                                          <> (if n ^. Node.isSelected then " node--selected" else []))
        , "style"       @= Aeson.object
            [ "transform" Aeson..= (transformTranslateToSvg offsetX offsetY)
            , "zIndex"    Aeson..= (show z)
            ]
        ] $ do
        svg_
            [ "className" $= "node__selection-mark"
            , "key"       $= "selection-mark"
            ] $ rect_ def mempty
        nodeProperties_ ref $ Properties.fromNode n
        div_
            [ "key"       $= "visualization"
            , "className" $= "node__visuals"
            ] $ forM_ (n ^. Node.value) visualization_
        svg_
            [ "key"       $= "essentials"
            , "className" $= "node__essentials"
            ] $ do
            text_
                [ "key"         $= "name"
                , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeId)
                , "className"   $= "node__name"
                , "y"           $= "-36"
                , "style"       @= Aeson.object
                    [ "fontSize" Aeson..= (fontSize)
                    ]
                ] $ elemString $ Text.unpack $ n ^. Node.expression
            if  n ^. Node.isExpanded then do
                ports         ref $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
                portsExpanded ref $ filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts
            else do
                ports ref $ filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts
                ports ref $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts

node_ :: Ref App -> Node -> Matrix Double -> ReactElementM ViewEventHandler ()
node_ ref model camTransform  = React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model, camTransform ) mempty
