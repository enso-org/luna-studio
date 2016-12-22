{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node where

import qualified Data.Aeson                           as Aeson
import qualified Data.Map.Lazy                        as Map
import qualified Data.Text.Lazy                       as Text
import           Luna.Studio.Data.Vector              (x, y)
import           Luna.Studio.Prelude
import           React.Flux
import qualified React.Flux                           as React
import           Empire.API.Data.Node                 (NodeId)
import           Empire.API.Data.Port                 (InPort (..), PortId (..))
import qualified Event.UI                             as UI
import qualified Luna.Studio.React.Event.Node         as Node
import           Luna.Studio.React.Model.Node         (Node)
import qualified Luna.Studio.React.Model.Node         as Node
import           Luna.Studio.React.Store              (Ref, dispatch, dt)
import           Luna.Studio.React.View.Global
import           Luna.Studio.React.View.ForeignObject (foreignObject_)
import           Luna.Studio.React.View.Global
import           Luna.Studio.React.View.Port          (port_)
import           Luna.Studio.React.View.PortControl   (portControl_)
import           Luna.Studio.React.View.Visualization (strValue, visualization_)
import           Object.Widget.Port                   (Port (..))
import qualified Object.Widget.Port                   as Port


objName :: JSString
objName = "node-editor"

makePorts :: Ref Node -> NodeId -> [Port] -> ReactElementM ViewEventHandler ()
makePorts nodeRef nodeId ports = forM_ ports $ \port -> port_ nodeRef nodeId port (countSameTypePorts port ports) (isPortSingle port ports)


node :: Ref Node -> ReactView ()
node ref = React.defineControllerView
    objName ref $ \nodeStore () -> do
        let n         = nodeStore ^. dt
            nodeId    = n ^. Node.nodeId
            pos       = n ^. Node.position
            ports     = Map.elems $ n ^. Node.ports
            offsetX   = show (pos ^. x)
            offsetY   = show (pos ^. y)
        if n ^. Node.isExpanded then
             div_
                 [ onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
                 , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
                 , onMouseDown   $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeId)
                 , "className" $= (fromString $ "node node--collapsed" <> (if n ^. Node.isSelected then " node--selected" else []))
                 , "style"     @= Aeson.object [ "transform" Aeson..= (transformTranslate offsetX offsetY) ]
                 , "key"       $= fromString (show nodeId)
                 ] $ do
                    svg_ [ "viewBox" $= "0 0 10 10" ] $ do
                        circle_ [ "className" $= "selection-mark" ] mempty
                        makePorts ref nodeId $ filter (\port -> port ^. Port.portId /= InPortId Self) ports
                        makePorts ref nodeId $ filter (\port -> port ^. Port.portId == InPortId Self) ports

                    div_ [ "className" $= "node-expanded" ]$ do
                        div_ [ "className" $= "name"] $
                            elemString $ Text.unpack (n ^. Node.expression)
                        div_ [ "className" $= "properties" ]$ do
                            div_ [ "className" $= "label" ] $ elemString "Name"
                            div_
                                [ "className" $= "value"
                                , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.NameEditStart nodeId
                                ] $
                                case n ^. Node.nameEdit of
                                    Just name ->
                                        input_
                                            [ "id" $= "focus-nameLabel"
                                            , "value" $= fromString (Text.unpack name)
                                            , onMouseDown $ \e _ -> [stopPropagation e]
                                            , onKeyDown   $ \e k ->  stopPropagation e : dispatch ref (UI.NodeEvent $ Node.NameKeyDown k nodeId)
                                            , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.NodeEvent $ Node.NameChange (fromString val) nodeId
                                            ]
                                    Nothing ->
                                        elemString $ fromString $ Text.unpack $ n ^. Node.name
                            -- forM_ (n ^. Node.ports) $ portControl_ n
                            div_ [ "className" $= "label" ] $ elemString "Display result"
                            div_ [ "className" $= "value" ] $
                                label_ ["className" $= "switch"] $ do
                                    input_ ["type" $= "checkbox"
                                           ,"value" $= if n ^. Node.visualizationsEnabled then "on" else "off"
                                           , onChange $ \e -> let val = not $ bool $ target e "value" in dispatch ref $ UI.NodeEvent $ Node.DisplayResultChanged val nodeId
                                           ]
                                    div_ ["className" $= "slider"] mempty
                            withJust (n ^. Node.execTime) $ \execTime -> do
                                div_ ["className" $= "label"] $ elemString "Execution time"
                                div_ ["className" $= "value"] $ elemString $ show execTime <> " ms"
                        div_ [ "className" $= "value"] $
                            elemString $ strValue n
                        div_ [ "className" $= "visualizations" ] $
                            forM_ (n ^. Node.value) visualization_

        else
            div_
                [ onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
                , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
                , onMouseDown   $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeId)
                , "className" $= (fromString $ "node node--collapsed" <> (if n ^. Node.isSelected then " node--selected" else []))
                , "style"     @= Aeson.object [ "transform" Aeson..= (transformTranslate offsetX offsetY) ]
                , "key"       $= fromString (show nodeId)
                ] $ do
                    svg_ [ "viewBox" $= "0 0 10 10" ] $ do
                        circle_ [ "className" $= "selection-mark" ] mempty
                        makePorts ref nodeId $ filter (\port -> (port ^. Port.portId) /= InPortId Self) ports
                        makePorts ref nodeId $ filter (\port -> (port ^. Port.portId) == InPortId Self) ports
                        text_
                            [ onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeId)
                            , "className"  $= "name"
                            , "y"          $= "-36"
                            ] $ elemString $ Text.unpack $ n ^. Node.expression
                        text_
                            [ "className"  $= "name"
                            , "y"          $= "45"
                            ] $ elemString $ strValue n



node_ :: Ref Node -> ReactElementM ViewEventHandler ()
node_ ref = React.view (node ref) () mempty

foreign import javascript safe "document.getElementById('focus-nameLabel').focus()" focusNameLabel :: IO ()


bool :: String -> Bool
bool "on" = True
bool _    = False
