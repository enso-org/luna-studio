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
import           JS.Scene                     (inputSidebarId, outputSidebarId)
import           Luna.Studio.Action.Geometry  (getPortNumber, isPortInput, lineHeight)
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
import qualified Luna.Studio.React.View.Style as Style
import           React.Flux                   hiding (view)


name :: Node -> JSString
name node = "edgeSidebar" <> if isInputEdge node then "Inputs" else "Outputs"

sendAddPortEvent :: Ref App -> Node -> [SomeStoreAction]
sendAddPortEvent ref node = dispatch ref (UI.EdgeEvent $ Edge.AddPort (node ^. Node.nodeId))

edgeSidebar_ :: Ref App -> Maybe DraggedPort -> Node -> ReactElementM ViewEventHandler ()
edgeSidebar_ ref mayDraggedPort node = when (isEdge node) $ do
    let ports   = node ^. Node.ports . to Map.elems
        nodeId  = node ^. Node.nodeId
        isPortDragged = Just nodeId == (view ( Port.draggedPort
                                             . Port.portRef
                                             . PortRef.nodeId) <$> mayDraggedPort)
    div_
        [ "className" $= Style.prefixFromList [ "edge-sidebar", if isInputEdge node then "edge-sidebar--i" else "edge-sidebar--o" ]
        , "key"       $= name node
        , "id"        $= if isInputEdge node then inputSidebarId else outputSidebarId
        , onMouseDown $ \e _ -> [stopPropagation e]
        , onMouseMove $ \e m -> stopPropagation e : (dispatch ref $ UI.EdgeEvent $ Edge.MouseMove m nodeId)
        ] $ do
        svg_ [] $ forM_ ports $ edgePort_ ref
        when (isInputEdge node) $ if isPortDragged then do
                div_
                    [ "className" $= Style.prefixFromList [ "edge__buton", "edge__button--remove", "noselect" ]
                    , "key"       $= (name node <> "RemoveButton")
                    , onMouseUp   $ \e _ -> stopPropagation e : (dispatch ref $ UI.EdgeEvent $ Edge.RemovePort)
                    ] $ elemString "Remove"
                withJust mayDraggedPort $ edgeDraggedPort_ ref
            else div_
                    [ "className" $= Style.prefixFromList [ "edge__buton", "edge__button--add", "noselect" ]
                    , "key"       $= (name node <> "AddButton")
                    , onMouseDown $ \e _ -> [stopPropagation e]
                    , onClick $ \e _ -> stopPropagation e : sendAddPortEvent ref node
                    ] $ elemString "Add"

edgePort_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
edgePort_ ref p = when (p ^. Port.visible) $ do
    let portRef   = p ^. Port.portRef
        portId    = p ^. Port.portId
        color     = convert $ p ^. Port.color
        num       = getPortNumber p
        highlight = if p ^. Port.highlight then ["hover"] else []
        classes   = if isPortInput p then [ "port", "port--i", "port--i--" <> show (num + 1)] ++ highlight
                                     else [ "port", "port--o", "port--o--" <> show (num + 1)] ++ highlight
        yPos      = lineHeight * fromIntegral (num + if isPortInput p then 1 else 0)
    g_
        [ "className" $= Style.prefixFromList classes ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> jsShow num <> "a")
            , "fill"      $= color
            , "r"         $= jsShow2 3
            , "cy"        $= jsShow2 yPos
            ] mempty
        circle_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> jsShow num <> "b")
              , "r"         $= jsShow2 (lineHeight/1.5)
              , "cy"        $= jsShow2 yPos
              ]
            ) mempty

--TODO[JK]: Style this correctly
edgeDraggedPort_ :: Ref App -> DraggedPort -> ReactElementM ViewEventHandler ()
edgeDraggedPort_ _ref draggedPort = do
    let pos   = draggedPort ^. Port.positionInSidebar
        -- color = convert $ draggedPort ^. Port.draggedPort . Port.color
    div_
        [ "className" $= Style.prefixFromList [ "port", "port--dragged", "port__shape", "hover" ]
        , "style"     @= Aeson.object [ "transform" Aeson..= ( "translate(" <> show (pos ^. x) <> "px, " <> show (pos ^. y) <> "px)" ) ]
        ] $ mempty



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
