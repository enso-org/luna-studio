{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node.Body where

import qualified Data.Aeson                             as Aeson
import qualified Data.Map.Lazy                          as Map
import           Empire.API.Data.Port                   (InPort (..), PortId (..))
import           Luna.Studio.Action.Geometry            (countSameTypePorts, isPortSingle)
import           Luna.Studio.Data.Matrix                (translatePropertyValue2)
import qualified Luna.Studio.Event.UI                   as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node           as Node
import           Luna.Studio.React.Model.App            (App)
import qualified Luna.Studio.React.Model.Field          as Field
import           Luna.Studio.React.Model.Node           (Node)
import qualified Luna.Studio.React.Model.Node           as Node
import qualified Luna.Studio.React.Model.NodeProperties as Properties
import qualified Luna.Studio.React.Model.Port           as Port
import           Luna.Studio.React.Store                (Ref, dispatch)
import           Luna.Studio.React.View.Field           (multilineField_)
import           Luna.Studio.React.View.Style           (blurBackground_, selectionMark_)
import           Luna.Studio.React.View.Node.Properties (nodeProperties_)
import           Luna.Studio.React.View.Port            (portExpanded_, port_)
import qualified Luna.Studio.React.View.Style           as Style
import           Luna.Studio.React.View.Visualization   (visualization_)
import           React.Flux
import qualified React.Flux                             as React

objName :: JSString
objName = "node-body"

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
        , "className"   $= Style.prefixFromList ([ "node", if n ^. Node.isCollapsed then "node--collapsed" else "node--expanded" ]
                                                ++ (if n ^. Node.isSelected  then ["node--selected"]  else []))
        , "style"       @= Aeson.object [ "transform" Aeson..= translatePropertyValue2 pos ]
        ] $ do
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
                    Node.Collapsed -> ""
                    Node.Expanded -> nodeProperties_ ref $ Properties.fromNode n
                    Node.Editor   -> multilineField_ [] "editor"
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
            if  n ^. Node.isCollapsed then do
                ports $ filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts
                ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
            else do
                ports $ filter (\port -> (port ^. Port.portId) == InPortId Self) nodePorts
                forM_  (filter (\port -> (port ^. Port.portId) /= InPortId Self) nodePorts) $ \port -> portExpanded_ ref port

nodeBody_ :: Ref App -> Node -> ReactElementM ViewEventHandler ()
nodeBody_ ref model = React.viewWithSKey nodeBody objName (ref, model) mempty
