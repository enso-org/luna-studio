{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node.Body where

import qualified Data.Aeson                             as Aeson
import qualified Data.Map.Lazy                          as Map
import           Empire.API.Data.Node                   (NodeId)
import           Empire.API.Data.Port                   (InPort (..), PortId (..))
import           Luna.Studio.Action.Geometry            (countSameTypePorts, isPortSingle)
import           Luna.Studio.Data.Matrix                (translatePropertyValue2)
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
import           Luna.Studio.React.View.Field           (multilineField_)
import           Luna.Studio.React.View.Node.Elements   (blurBackground_, selectionMark_)
import           Luna.Studio.React.View.Node.Properties (nodeProperties_)
import           Luna.Studio.React.View.Port            (portExpanded_, port_)
import           Luna.Studio.React.View.Style           (lunaPrefix)
import           Luna.Studio.React.View.Visualization   (visualization_)
import           React.Flux
import qualified React.Flux                             as React


objName :: JSString
objName = "node-body"

handleMouseDown :: Ref App -> NodeId -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref nodeId e m =
    if Mouse.withoutMods m Mouse.leftButton
    then stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeId)
    else []

nodeBody :: ReactView (Ref App, Node)
nodeBody = React.defineView objName $ \(ref, n) -> do
    let nodeId    = n ^. Node.nodeId
        pos       = n ^. Node.position
        nodePorts = Map.elems $ n ^. Node.ports
        ports p   = forM_ p $ \port -> port_ ref port (countSameTypePorts port p) $ isPortSingle port p
    div_
        [ "key"         $= "nodeBodyRoot"
        , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
        , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
        , onMouseDown   $ handleMouseDown ref nodeId
        , "className"   $= (lunaPrefix "node " <> (if n ^. Node.isCollapsed then lunaPrefix "node--collapsed " else lunaPrefix "node--expanded ")
                                               <> (if n ^. Node.isSelected  then lunaPrefix "node--selected"  else ""))
        , "style"       @= Aeson.object [ "transform" Aeson..= translatePropertyValue2 pos ]
        ] $ do
        div_
            [ "key"       $= "main"
            , "className" $= lunaPrefix "node__main"
            ] $ do
            selectionMark_
            div_
                [ "key"       $= "properties-crop"
                , "className" $= lunaPrefix "node__properties-crop"
                ] $ do
                blurBackground_
                case n ^. Node.mode of
                    Node.Collapsed -> ""
                    Node.Expanded -> nodeProperties_ ref $ Properties.fromNode n
                    Node.Editor   -> multilineField_
                        [ onKeyDown   $ \e _ -> [stopPropagation e]
                        , onMouseDown $ \e _ -> [stopPropagation e]
                        , onChange  $ dispatch ref . UI.NodeEvent . Node.SetCode nodeId . (`target` "value")
                        ] ref "editor" $ fromMaybe def $ n ^. Node.code
        div_
            [ "key"       $= "visualization"
            , "className" $= lunaPrefix "node__visuals"
            ] $ forM_ (n ^. Node.value) $ visualization_ ref nodeId def
        svg_
            [ "key"       $= "essentials"
            , "className" $= lunaPrefix "node__essentials"
            ] $ do
            if  n ^. Node.isCollapsed then do
                ports $ filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts
                ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
            else do
                ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
                forM_  (filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts) $ \port -> portExpanded_ ref port

nodeBody_ :: Ref App -> Node -> ReactElementM ViewEventHandler ()
nodeBody_ ref model = React.viewWithSKey nodeBody objName (ref, model) mempty
