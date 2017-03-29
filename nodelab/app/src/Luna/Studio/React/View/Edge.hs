{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Luna.Studio.React.View.Edge
    ( edgeSidebar_
    , edgeDraggedPort_
    , focusPortLabel
    ) where

import qualified Data.Aeson                            as Aeson
import qualified Data.Map.Lazy                         as Map
import           Data.Position                         (y)
import qualified JS.Config                             as Config
import           JS.Scene                              (inputSidebarId, outputSidebarId)
import qualified JS.UI                                 as UI
import           Luna.Studio.Data.PortRef              (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef), toAnyPortRef)
import qualified Luna.Studio.Event.UI                  as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Edge          as Edge
import           Luna.Studio.React.Model.App           (App)
import           Luna.Studio.React.Model.Constants     (lineHeight)
import qualified Luna.Studio.React.Model.Field         as Field
import           Luna.Studio.React.Model.Node.EdgeNode (EdgeMode (AddRemove, MoveConnect), EdgeNode, NodeLoc, countProjectionPorts,
                                                        isInputEdge)
import qualified Luna.Studio.React.Model.Node.EdgeNode as EdgeNode
import           Luna.Studio.React.Model.Port          (OutPort (All, Projection), Port (..), getPortNumber, getPositionInSidebar,
                                                        isHighlighted, isInMovedMode, isInNameEditMode, isInPort, isOutPort)
import qualified Luna.Studio.React.Model.Port          as Port
import           Luna.Studio.React.Store               (Ref, dispatch)
import           Luna.Studio.React.View.Field          (singleField_)
import           Luna.Studio.React.View.Port           (handleClick, handleMouseDown, handleMouseUp, jsShow2)
import           Luna.Studio.React.View.Style          (plainPath, plainRect)
import qualified Luna.Studio.React.View.Style          as Style
import           React.Flux                            hiding (view)


name :: EdgeNode -> JSString
name node = "edgePorts" <> if isInputEdge node then "Inputs" else "Outputs"

portHandlers :: Ref App -> EdgeMode -> Bool -> AnyPortRef -> [PropertyOrHandler [SomeStoreAction]]
portHandlers ref AddRemove isOnly portRef =
    [ onMouseDown $ \e _ -> [stopPropagation e] ] ++
    if isOnly then [] else
    [ onClick     $ \e _ -> stopPropagation e : dispatch ref (UI.EdgeEvent $ Edge.RemovePort portRef) ]

portHandlers ref MoveConnect _ portRef =
    [ onMouseDown $ \e _ -> [stopPropagation e]
    , onClick     $ handleClick     ref portRef
    , onMouseDown $ handleMouseDown ref portRef
    , onMouseUp   $ handleMouseUp   ref portRef
    ]

edgeSidebar_ :: Ref App -> EdgeNode -> ReactElementM ViewEventHandler ()
edgeSidebar_ ref node = do
    let ports            = node ^. EdgeNode.ports . to Map.elems
        nodeLoc          = node ^. EdgeNode.nodeLoc
        mode             = node ^. EdgeNode.mode
        portDraggedClass = if any isInMovedMode ports then ["edgeports--dragmode"] else []
        classes          = [ "edgeports", if isInputEdge node then "edgeports--i" else "edgeports--o" ]
                        ++ if mode == AddRemove then ["edgeports--editmode"] else []
                        ++ portDraggedClass
    div_
        [ "key"         $= name node
        , "className"   $= Style.prefixFromList classes
        , onDoubleClick $ \e _ -> [stopPropagation e]
        , onMouseDown   $ \e _ -> [stopPropagation e]
        , onMouseMove   $ \e m -> stopPropagation e : (dispatch ref $ UI.EdgeEvent $ Edge.MouseMove m nodeLoc)
        ] $ do
        div_
            [ "key" $= "activeArea"
            , "className" $= Style.prefixFromList [ "edgeports__active-area", "noselect" ]
            ] $
            div_
                [ "key"       $= "EdgePortsBody"
                , "id"        $= if isInputEdge node then inputSidebarId else outputSidebarId
                , "className" $= Style.prefixFromList [ "edgeports__body", "noselect" ]
                ] $ do
                -- TODO[LJK]: Find out how self port works in this case
                -- when (isInputEdge node) $
                --     div_
                --         [ "className" $= Style.prefixFromList ["port", "edgeport", "edgeport--i", "edgeport--self"]
                --         ] $
                --         svg_
                --             [ "className" $= Style.prefix "edgeport__svg"
                --             ] $
                --             g_
                --                 [ "className" $= Style.prefixFromList [ "port", "port--self" ]
                --                 ] $ do
                --                 circle_
                --                     [ "className" $= Style.prefix "port__shape"
                --                     , "fill"      $= "rgba(119,119,119,255)"
                --                     , "r"         $= "5"
                --                     ] mempty
                --                 circle_
                --                     [ "className" $= Style.prefix "port__select"
                --                     , "r"         $= "13"
                --                     ] mempty

                forM_ ports $ \p -> if isInMovedMode p
                    then edgePlaceholderForPort_ >> edgeDraggedPort_ ref p
                    else edgePort_ ref mode nodeLoc (countProjectionPorts node == 1) p

                when (isInputEdge node) $ do
                    svg_
                        [ "className" $= Style.prefixFromList [ "edgeport__svg", "edgeport__svg--addbutton" ]
                        , "key"       $= (name node <> "AddButton")
                        , onMouseDown $ \e _ -> [stopPropagation e]
                        , onClick $ \e _ -> stopPropagation e : dispatch ref (UI.EdgeEvent $ Edge.AddPort $ OutPortRef' (OutPortRef nodeLoc (Projection (countProjectionPorts node) All)))
                        ] $ do
                        circle_
                            [ "className" $= Style.prefix "port__shape"
                            , "key"       $= jsShow "addButtonShape"
                            , "r"         $= jsShow2 3
                            ] mempty
                        g_ [ "className" $= Style.prefix "port__plus" ] $ do
                              plainRect 0 0 0 0
                              plainRect 0 0 0 0
                        circle_
                            [ "className" $= Style.prefix "port__select"
                            , "key"       $= jsShow "addButtonSelect"
                            , "r"         $= jsShow2 (lineHeight/1.5)
                            ] mempty
                    svg_
                        [ "className" $= Style.prefix "edit-icon"
                        , onClick $ \e _ -> stopPropagation e : dispatch ref (UI.EdgeEvent $ Edge.ToggleEdgeMode nodeLoc)
                        , "key"       $= (name node <> "editIcon")
                        ] $ do
                        circle_
                            [ "className" $= Style.prefix "edit-icon__shape01"
                            , "key"       $= jsShow "editIconShape"
                            , "r"         $= jsShow2 3
                            ] mempty
                        g_ [ "className" $= Style.prefix "edit-icon__shape02" ] $ do
                              plainPath "" ""
                              plainRect 0 0 0 0
                              plainRect 0 0 0 0
                        circle_
                            [ "className" $= Style.prefix "edit-icon__select"
                            , "key"       $= jsShow "editIconSelect"
                            , "r"         $= jsShow2 (lineHeight/1.5)
                            ] mempty

addButton_ :: Ref App -> AnyPortRef -> ReactElementM ViewEventHandler ()
addButton_ ref portRef =
    svg_
        [ "className" $= Style.prefixFromList ["edgeport__svg", "edgeport__svg--inbetween"]
        , onMouseDown $ \e _ -> [stopPropagation e]
        , onClick     $ \e _ -> stopPropagation e : dispatch ref (UI.EdgeEvent $ Edge.AddPort portRef)
        ] $
        g_ [ "className" $= Style.prefix "port-add-inbetween" ] $ do
            g_ [ "className" $= Style.prefix "port-add-inbetween__shape" ] $ do
                plainPath (Style.prefix "port-add-inbetween__droplet") "M10.0749836,12.9509892 C11.4541267,14.1514559 13.0835452,14.9902759 15.0097241,14.9902759 C18.8703469,14.9902759 22,11.8606228 22,8 C22,4.13937722 18.8703469,1.0097241 15.0097241,1.0097241 C13.0977164,1.0097241 11.4518168,1.82232527 10.1029674,3.02127407 C5.44945277,7.13675725 4.06697429,7.99999996 1.05578798,7.99999996 C4.06697429,7.99999996 5.38818292,8.87139207 10.0749836,12.9509892 Z"
                g_ [ "className" $= Style.prefix "port-add-inbetween__plus" ] $ do
                    plainRect 2 8 (-1) (-4)
                    plainRect 8 2 (-4) (-1)
            plainPath (Style.prefix "port-add-inbetween__selectable") "M 20 0 A 10 10 0 0 1 20 16 L 10 16 A 10 10 0 0 1 10 0 Z"

edgePort_ :: Ref App -> EdgeMode -> NodeLoc -> Bool -> Port -> ReactElementM ViewEventHandler ()
edgePort_ ref mode nl isOnly p = do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nl portId
        color     = convert $ p ^. Port.color
        num       = getPortNumber portId
        highlight = if isHighlighted p || isInNameEditMode p then [ "hover" ] else []
        classes   = if isInPort portId then [ "port", "edgeport", "edgeport--o", "edgeport--o--" <> show (num + 1) ] ++ highlight
                                       else [ "port", "edgeport", "edgeport--i", "edgeport--i--" <> show (num + 1) ] ++ highlight
    div_
        [ "key"       $= ( jsShow portId <> "-port-" <> jsShow num )
        , "className" $= Style.prefixFromList classes
        ] $ do
        when (isOutPort portId) $ addButton_ ref portRef
        svg_
            [ "className" $= Style.prefix "edgeport__svg"
            ] $ do
            circle_
                [ "className" $= Style.prefix "port__shape"
                , "key"       $= (jsShow portId <> jsShow num <> "a")
                , "fill"      $= color
                , "r"         $= jsShow2 3
                ] mempty
            when (not isOnly) $ g_ [ "className" $= Style.prefix "port__plus" ] $ do
                  plainRect 2 8 (-1) (-4)
                  plainRect 8 2 (-4) (-1)
            circle_ (
                [ "className" $= Style.prefix "port__select"
                , "key"       $= (jsShow portId <> jsShow num <> "b")
                , "r"         $= jsShow2 (lineHeight/1.5)
                ] ++ portHandlers ref mode isOnly portRef ) mempty

        if isInNameEditMode p then
            singleField_ [ "id" $= portLabelId ] (jsShow portId)
                $ Field.mk ref (convert $ p ^. Port.name)
                & Field.onCancel .~ Just (const $ UI.EdgeEvent $ Edge.PortNameDiscard portRef)
                & Field.onAccept .~ Just (UI.EdgeEvent . Edge.PortNameApply portRef . convert)
        else
            div_ [ "className" $= Style.prefixFromList ["edgeport__name", "noselect"]
                 , onDoubleClick $ \_ _ -> dispatch ref $ UI.EdgeEvent $ Edge.PortNameStartEdit portRef
                 ] $ elemString $ p ^. Port.name

edgePlaceholderForPort_ :: ReactElementM ViewEventHandler ()
edgePlaceholderForPort_ = div_
    [ "key"       $= "port-placeholder"
    , "className" $= Style.prefixFromList [ "port", "edgeport", "edgeport--i", "noselect" ]
    ] mempty

edgeDraggedPort_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
edgeDraggedPort_ _ref p = withJust (getPositionInSidebar p) $ \pos ->
    div_
        [ "className" $= Style.prefixFromList [ "port", "edgeport", "edgeport--dragged", "hover" ]
        , "style"     @= Aeson.object [ "transform"  Aeson..= ("translate(0px, " <> show (pos ^. y) <> "px)") ]
        ] $ do
        div_ [ "className" $= Style.prefix "edgeport__name" ] $ elemString $ p ^. Port.name
        svg_
            [ "className" $= Style.prefix "edgeport__svg" ] $
            circle_
                [ "className" $= Style.prefix "port__shape"
                , "r"         $= jsShow2 3
                ] mempty

portLabelId :: JSString
portLabelId = Config.prefix "focus-portLabel"

focusPortLabel :: IO ()
focusPortLabel = UI.focus portLabelId

-- TODO[PM]: Findout why MouseEnter and MouseLeave does not work correctly
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- module Luna.Studio.React.View.Edge
--     ( edgeSidebar_
--     , edgeDraggedPort_
--     ) where
--
-- import qualified Data.Aeson                      as Aeson
-- import qualified Data.Map.Lazy                as Map
-- import           Data.Position                (x, y)
-- import qualified Empire.API.Data.PortRef      as PortRef
-- import           Luna.Studio.Action.Geometry  (getPortNumber, isPortInput, lineHeight)
-- import qualified Luna.Studio.Event.UI         as UI
-- import           Luna.Studio.Prelude
-- import qualified Luna.Studio.React.Event.Edge as Edge
-- import           Luna.Studio.React.Model.App  (App)
-- import           Luna.Studio.React.Model.Node (Node, isEdge, isInputEdge)
-- import qualified Luna.Studio.React.Model.Node as Node
-- import           Luna.Studio.React.Model.Port (DraggedPort, Port (..))
-- import qualified Luna.Studio.React.Model.Port as Port
-- import           Luna.Studio.React.Store      (Ref, dispatch)
-- import           Luna.Studio.React.View.Port  (handlers, jsShow2)
-- import           React.Flux                   hiding (view)
-- import qualified React.Flux as React
-- import qualified JS.Config                       as Config
--
--
-- name :: Node -> JSString
-- name node = fromString $ "edgeSidebar" <> if isInputEdge node then "Inputs" else "Outputs"
--
-- key :: Node -> JSString
-- key node = fromString $ "edge-sidebar--" <> if isInputEdge node then "i" else "o"
--
-- keyWithPrefix :: Node -> JSString
-- keyWithPrefix = Config.prefix . key
--
-- portName :: JSString
-- portName = "edgePort"
--
--
-- edgeSidebar :: JSString -> ReactView (Ref App, Maybe DraggedPort, Node)
-- edgeSidebar name' = React.defineView name' $ \(ref, mayDraggedPort, node) -> do
--     let classes = "luna-edge-sidebar luna-" <> key node
--         ports   = node ^. Node.ports . to Map.elems
--         nodeId  = node ^. Node.nodeId
--         isPortDragged = Just nodeId == (view ( Port.draggedPort
--                                              . Port.portRef
--                                              . PortRef.nodeId) <$> mayDraggedPort)
--     div_
--         [ "className" $= classes
--         , "key"       $= name node
--         , onMouseDown $ \e _ -> [stopPropagation e]
--         , onMouseMove $ \e m -> stopPropagation e : (dispatch ref $ UI.EdgeEvent $ Edge.MouseMove m nodeId)
--         ] $ do
--         svg_ [] $ forM_ ports $ edgePort_ ref $ if isPortDragged then mayDraggedPort else Nothing
--         when (isInputEdge node) $ if isPortDragged then do
--                 div_
--                     [ "className" $= "luna-edge__buton luna-edge__button--remove luna-noselect"
--                     , "key"       $= (name node <> "RemoveButton")
--                     , onMouseUp   $ \e _ -> stopPropagation e : (dispatch ref $ UI.EdgeEvent $ Edge.RemovePort)
--                     ] $ elemString "Remove"
--                 withJust mayDraggedPort edgeDraggedPort_
--             else div_
--                     [ "className" $= "luna-edge__buton luna-edge__button--add luna-noselect"
--                     , "key"       $= (name node <> "AddButton")
--                     , onMouseDown $ \e _ -> [stopPropagation e]
--                     , onClick $ \e _ -> stopPropagation e : (dispatch ref (UI.EdgeEvent $ Edge.AddPort nodeId))
--                     ] $ elemString "Add"
--
-- edgeSidebar_ :: Ref App -> Maybe DraggedPort -> Node -> ReactElementM ViewEventHandler ()
-- edgeSidebar_ ref mayDraggedPort node = when (isEdge node) $
--     React.viewWithSKey (edgeSidebar $ name node) (key node) (ref, mayDraggedPort, node) mempty
--
-- --TODO[LJK]: Decide what should happend when port is far away
-- getPortYPos :: Maybe DraggedPort -> Port -> Double
-- getPortYPos mayDraggedPort p = do
--     let num = getPortNumber p
--         originalYPos = lineHeight * fromIntegral (num + if isPortInput p then 1 else 0)
--     case mayDraggedPort of
--         Nothing          -> originalYPos
--         Just draggedPort -> do
--             let draggedPortYPos = draggedPort ^. Port.positionInSidebar . y
--                 draggedPortNum  = getPortNumber $ draggedPort ^. Port.draggedPort
--                 shift1          = if num < draggedPortNum then 0 else (-lineHeight)
--                 shift2          = if originalYPos + shift1 < draggedPortYPos - lineHeight / 2 then 0 else lineHeight
--             originalYPos + shift1 + shift2
--
-- edgePort :: ReactView (Ref App, Maybe DraggedPort, Port)
-- edgePort = React.defineView portName $ \(ref, mayDraggedPort, p) -> do
--     let portRef   = p ^. Port.portRef
--         portId    = p ^. Port.portId
--         color     = convert $ p ^. Port.color
--         num       = getPortNumber p
--         highlight = if p ^. Port.highlight then " luna-hover" else ""
--         classes   = if isPortInput p then "luna-port luna-port--i luna-port--i--" else "luna-port luna-port--o luna-port--o--"
--         className = fromString $ classes <> show (num + 1) <> highlight
--         yPos      = getPortYPos mayDraggedPort p
--     g_
--         [ "className" $= className ] $ do
--         circle_
--             [ "className" $= "luna-port__shape"
--             , "key"       $= (jsShow portId <> jsShow num <> "a")
--             , "fill"      $= color
--             , "r"         $= jsShow2 3
--             , "cy"        $= jsShow2 yPos
--             ] mempty
--         circle_
--             ( handlers ref portRef ++
--               [ "className" $= "luna-port__select"
--               , "key"       $= (jsShow portId <> jsShow num <> "b")
--               , "r"         $= jsShow2 (lineHeight/1.5)
--               , "cy"        $= jsShow2 yPos
--               ]
--             ) mempty
--
-- edgePort_ :: Ref App -> Maybe DraggedPort -> Port -> ReactElementM ViewEventHandler ()
-- edgePort_ ref mayDraggedPort p = when (p ^. Port.visible) $
--     React.viewWithSKey edgePort (jsShow $ p ^. Port.portId) (ref, mayDraggedPort, p) mempty
--
-- -- TODO[JK]: Style this correctly
-- edgeDraggedPort :: ReactView DraggedPort
-- edgeDraggedPort = React.defineView portName $ \draggedPort -> do
--     let pos   = draggedPort ^. Port.positionInSidebar
--         -- color = convert $ draggedPort ^. Port.draggedPort . Port.color
--
--     div_
--         [ "className" $= "luna-port luna-port--dragged luna-hover luna-port__shape"
--         , "style"     @= Aeson.object [ "transform" Aeson..= ( "translate(" <> show (pos ^. x) <> "px, " <> show (pos ^. y) <> "px)" ) ]
--         ] $ mempty
--
--
-- edgeDraggedPort_ :: DraggedPort -> ReactElementM ViewEventHandler ()
-- edgeDraggedPort_ draggedPort = React.viewWithSKey edgeDraggedPort "draggedPort" draggedPort mempty
