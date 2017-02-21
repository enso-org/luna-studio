{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Luna.Studio.React.View.Edge
    ( edgeSidebar_
    , edgeDraggedPort_
    ) where

import qualified Data.Aeson                   as Aeson
import qualified Data.Map.Lazy                as Map
import           Data.Position                (x, y)
import qualified Empire.API.Data.PortRef      as PortRef
import           Luna.Studio.Action.Geometry  (getPortNumber, isPortInput, lineHeight)
import           Luna.Studio.Data.Color       (toJSString)
import qualified Luna.Studio.Event.UI         as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Edge as Edge
import           Luna.Studio.React.Model.App  (App)
import           Luna.Studio.React.Model.Node (Node, isEdge, isInputEdge)
import qualified Luna.Studio.React.Model.Node as Node
import           Luna.Studio.React.Model.Port (DraggedPort, Port (..))
import qualified Luna.Studio.React.Model.Port as Port
import           Luna.Studio.React.Store      (Ref, dispatch)
import           Luna.Studio.React.View.Port  (handlers, jsShow2)
import           React.Flux


name :: Node -> String
name node = "edgeSidebar" <> if isInputEdge node then "Inputs" else "Outputs"

sendAddPortEvent :: Ref App -> Node -> [SomeStoreAction]
sendAddPortEvent ref node = dispatch ref (UI.EdgeEvent $ Edge.AddPort (node ^. Node.nodeId))

edgeSidebar_ :: Ref App -> Maybe DraggedPort -> Node -> ReactElementM ViewEventHandler ()
edgeSidebar_ ref mayDraggedPort node = when (isEdge node) $ do
    let classes = "luna-edge-sidebar luna-edge-sidebar" <> if isInputEdge node then "--i" else "--o" <> " luna-noselect"
        ports   = node ^. Node.ports . to Map.elems
        nodeId  = node ^. Node.nodeId
    div_
        [ "className" $= classes
        , "key"       $= (fromString $ name node)
        , onMouseDown $ \e _ -> [stopPropagation e]
        , onMouseMove $ \e m -> stopPropagation e : (dispatch ref $ UI.EdgeEvent $ Edge.MouseMove m nodeId)
        ] $ do
        svg_ [] $ forM_ ports $ edgePort_ ref node
        when (isInputEdge node) $ do
            div_
                [ "className" $= "luna-edge__buton luna-edge__button--add"
                , "key"       $= (fromString $ name node <> "AddButton")
                , onMouseDown $ \e _ -> [stopPropagation e]
                , onClick $ \e _ -> stopPropagation e : sendAddPortEvent ref node
                ] $ elemString "Add"
            div_
                [ "className" $= "luna-edge__buton luna-edge__button--remove"
                , "key"       $= (fromString $ name node <> "RemoveButton")
                , onMouseDown $ \e _ -> [stopPropagation e]
                , onMouseUp   $ \_ _ -> dispatch ref $ UI.EdgeEvent $ Edge.RemovePort
                ] $ elemString "Remove"
        withJust mayDraggedPort $ \draggedPort ->
            when (draggedPort ^. Port.draggedPort . Port.portRef . PortRef.nodeId == nodeId) $
                edgeDraggedPort_ ref draggedPort


edgePort_ :: Ref App -> Node -> Port -> ReactElementM ViewEventHandler ()
edgePort_ ref _n p = when (p ^. Port.visible) $ do
    let portRef   = p ^. Port.portRef
        portId    = p ^. Port.portId
        isInput   = isPortInput p
        num       = getPortNumber p
        color     = toJSString $ p ^. Port.color
        highlight = if p ^. Port.highlight then " luna-hover" else ""
        classes   = if isInput then "luna-port luna-port--i luna-port--i--" else "luna-port luna-port--o luna-port--o--"
        className = fromString $ classes <> show (num + 1) <> highlight
        k         = if isInput then 1 else 0
    g_
        [ "className" $= className ] $ do
        circle_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> jsShow num <> "a")
            , "fill"      $= color
            , "r"         $= jsShow2 3
            , "cy"        $= jsShow2 (lineHeight * fromIntegral (num + k) )
            ] mempty
        circle_
            ( handlers ref portRef ++
              [ "className" $= "luna-port__select"
              , "key"       $= (jsShow portId <> jsShow num <> "b")
              , "r"         $= jsShow2 (lineHeight/1.5)
              , "cy"        $= jsShow2 (lineHeight * fromIntegral (num + k) )
              ]
            ) mempty

edgeDraggedPort_ :: Ref App -> DraggedPort -> ReactElementM ViewEventHandler ()
edgeDraggedPort_ _ref draggedPort = do
    let color = toJSString $ draggedPort ^. Port.draggedPort . Port.color
        pos   = draggedPort ^. Port.position
    svg_
        [ "className" $= "luna-port luna-port--dragged luna-hover"
        , "style"     @= Aeson.object [ "transform" Aeson..= ( "translate(" <> show (pos ^. x) <> "px, " <> show (pos ^. y) <> "px)" ) ]
        ] $ do
        circle_
            [ "className" $= "luna-port__shape"
            , "key"       $= "draggedPort"
            , "fill"      $= color
            , "r"         $= jsShow2 3
--            , "cx"        $= fromString (show $ pos ^. x)
--            , "cy"        $= fromString (show $ pos ^. y)
            ] mempty
