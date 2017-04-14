{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Luna.Studio.React.View.Sidebar
    ( sidebar_
    , focusPortLabel
    ) where

import qualified Data.Aeson                               as Aeson
import           Data.Position                            (y)
import           Empire.API.Data.PortRef                  (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef), toAnyPortRef)
import qualified JS.Config                                as Config
import           JS.Scene                                 (inputSidebarId, outputSidebarId)
import qualified JS.UI                                    as UI
import qualified Luna.Studio.Event.UI                     as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Sidebar          as Sidebar
import           Luna.Studio.React.Model.App              (App)
import           Luna.Studio.React.Model.Constants        (lineHeight)
import qualified Luna.Studio.React.Model.Field            as Field
import           Luna.Studio.React.Model.Node.SidebarNode (NodeLoc, SidebarMode (AddRemove, MoveConnect), SidebarNode, countProjectionPorts,
                                                           isInputSidebar)
import qualified Luna.Studio.React.Model.Node.SidebarNode as SidebarNode
import           Luna.Studio.React.Model.Port             (AnyPort, OutPortIndex (Projection), getPortNumber, getPositionInSidebar,
                                                           isHighlighted, isInMovedMode, isInNameEditMode, isInPort, isOutPort)
import qualified Luna.Studio.React.Model.Port             as Port
import           Luna.Studio.React.Store                  (Ref, dispatch)
import           Luna.Studio.React.View.Field             (singleField_)
import           Luna.Studio.React.View.Port              (handleClick, handleMouseDown, handleMouseUp, jsShow2)
import           Luna.Studio.React.View.Style             (plainPath_, plainRect_)
import qualified Luna.Studio.React.View.Style             as Style
import           React.Flux                               hiding (view)


name :: SidebarNode node => node -> JSString
name node = "sidebarPorts" <> if isInputSidebar node then "Inputs" else "Outputs"

portHandlers :: Ref App -> SidebarMode -> Bool -> Bool -> AnyPortRef -> [PropertyOrHandler [SomeStoreAction]]
portHandlers ref AddRemove _ isOnly portRef =
    [ onMouseDown $ \e _ -> [stopPropagation e] ] ++
    if isOnly then [] else
    [ onClick     $ \e _ -> stopPropagation e : dispatch ref (UI.SidebarEvent $ Sidebar.RemovePort portRef) ]

portHandlers ref MoveConnect False _ portRef =
    [ onMouseDown $ \e _ -> [stopPropagation e]
    , onClick     $ handleClick     ref portRef
    , onMouseDown $ handleMouseDown ref portRef
    , onMouseUp   $ handleMouseUp   ref portRef
    ]
portHandlers _ _ _ _ _ = []

sidebar_ :: SidebarNode node => Ref App -> node -> ReactElementM ViewEventHandler ()
sidebar_ ref node = do
    let ports             = SidebarNode.portsList node
        nodeLoc           = node ^. SidebarNode.nodeLoc
        mode              = node ^. SidebarNode.mode
        mayFixedPos       = map (\pos -> "style" @= Aeson.object [ "bottom" Aeson..= show pos]) $
            maybeToList $ node ^. SidebarNode.fixedBottomPos
        isPortDragged     = any isInMovedMode ports
        classes           = [ "sidebar", if isInputSidebar node then "sidebar--i" else "sidebar--o" ]
                         ++ if mode == AddRemove then ["sidebar--editmode"] else []
                         ++ if isPortDragged then ["sidebar--dragmode"] else []
        addButtonHandlers = let portRef = OutPortRef' (OutPortRef nodeLoc [Projection (countProjectionPorts node)]) in
            case mode of
                AddRemove   -> [ onMouseDown $ \e _ -> [stopPropagation e]
                               , onClick     $ \e _ -> stopPropagation e : dispatch ref (UI.SidebarEvent $ Sidebar.AddPort portRef)
                               ]
                MoveConnect -> portHandlers ref mode False False portRef


    div_ (
        [ "key"         $= name node
        , "className"   $= Style.prefixFromList classes
        , onDoubleClick $ \e _ -> [stopPropagation e]
        , onMouseDown   $ \e _ -> [stopPropagation e]
        , onMouseMove   $ \e m -> stopPropagation e : (dispatch ref $ UI.SidebarEvent $ Sidebar.MouseMove m nodeLoc)
        ] ++ mayFixedPos ) $ do
        div_
            [ "key" $= "activeArea"
            , "className" $= Style.prefixFromList [ "sidebar__active-area", "noselect" ]
            ] $
            div_
                [ "key"       $= "SidebarPortsBody"
                , "id"        $= if isInputSidebar node then inputSidebarId else outputSidebarId
                , "className" $= Style.prefixFromList [ "sidebar__body", "noselect" ]
                ] $ do
                -- TODO[LJK]: Find out how self port works in this case
                -- when (isInputSidebar node) $
                --     div_
                --         [ "className" $= Style.prefixFromList ["port", "sidebarport", "sidebarport--i", "sidebarport--self"]
                --         ] $
                --         svg_
                --             [ "className" $= Style.prefix "sidebarport__svg"
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
                    then sidebarPlaceholderForPort_ >> sidebarDraggedPort_ ref p
                    else sidebarPort_ ref mode nodeLoc isPortDragged (countProjectionPorts node == 1) p

                when (isInputSidebar node && not isPortDragged) $ do
                    svg_ (
                        [ "className" $= Style.prefixFromList [ "sidebar__port__svg", "sidebar__port__svg--addbutton" ]
                        , "key"       $= (name node <> "AddButton")
                        ] ++ addButtonHandlers ) $ do
                        circle_
                            [ "className" $= Style.prefix "port__shape"
                            , "key"       $= jsShow "addButtonShape"
                            , "r"         $= jsShow2 3
                            ] mempty
                        g_ [ "className" $= Style.prefix "port__plus" ] $ do
                              plainRect_ "key1" 0 0 0 0
                              plainRect_ "key2" 0 0 0 0
                        circle_
                            [ "className" $= Style.prefix "port__select"
                            , "key"       $= jsShow "addButtonSelect"
                            , "r"         $= jsShow2 (lineHeight/1.5)
                            ] mempty
                    svg_
                        [ "className" $= Style.prefix "edit-icon"
                        , onClick $ \e _ -> stopPropagation e : dispatch ref (UI.SidebarEvent $ Sidebar.ToggleInputMode nodeLoc)
                        , "key"       $= (name node <> "editIcon")
                        ] $ do
                        circle_
                            [ "className" $= Style.prefix "edit-icon__shape01"
                            , "key"       $= jsShow "editIconShape"
                            , "r"         $= jsShow2 3
                            ] mempty
                        g_ [ "className" $= Style.prefix "edit-icon__shape02" ] $ do
                              plainPath_ "" ""
                              plainRect_ "key1" 0 0 0 0
                              plainRect_ "key2" 0 0 0 0
                        circle_
                            [ "className" $= Style.prefix "edit-icon__select"
                            , "key"       $= jsShow "editIconSelect"
                            , "r"         $= jsShow2 (lineHeight/1.5)
                            ] mempty

addButton_ :: Ref App -> AnyPortRef -> ReactElementM ViewEventHandler ()
addButton_ ref portRef =
    svg_
        [ "className" $= Style.prefixFromList ["sidebar__port__svg", "sidebar__port__svg--inbetween"]
        , onMouseDown $ \e _ -> [stopPropagation e]
        , onClick     $ \e _ -> stopPropagation e : dispatch ref (UI.SidebarEvent $ Sidebar.AddPort portRef)
        ] $
        g_ [ "className" $= Style.prefix "port-add-inbetween" ] $ do
            g_ [ "className" $= Style.prefix "port-add-inbetween__shape" ] $ do
                plainPath_ (Style.prefix "port-add-inbetween__droplet") "M10.0749836,12.9509892 C11.4541267,14.1514559 13.0835452,14.9902759 15.0097241,14.9902759 C18.8703469,14.9902759 22,11.8606228 22,8 C22,4.13937722 18.8703469,1.0097241 15.0097241,1.0097241 C13.0977164,1.0097241 11.4518168,1.82232527 10.1029674,3.02127407 C5.44945277,7.13675725 4.06697429,7.99999996 1.05578798,7.99999996 C4.06697429,7.99999996 5.38818292,8.87139207 10.0749836,12.9509892 Z"
                g_ [ "className" $= Style.prefix "port-add-inbetween__plus" ] $ do
                    plainRect_ "key1" 2 8 (-1) (-4)
                    plainRect_ "key2" 8 2 (-4) (-1)
            plainPath_ (Style.prefix "port-add-inbetween__selectable") "M 20 0 A 10 10 0 0 1 20 16 L 10 16 A 10 10 0 0 1 10 0 Z"

sidebarPort_ :: Ref App -> SidebarMode -> NodeLoc -> Bool -> Bool -> AnyPort -> ReactElementM ViewEventHandler ()
sidebarPort_ ref mode nl isPortDragged isOnly p = do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nl portId
        color     = convert $ p ^. Port.color
        num       = getPortNumber portId
        highlight = if isHighlighted p || isInNameEditMode p then [ "hover" ] else []
        classes   = if isInPort portId then [ "port", "sidebar__port", "sidebar__port--o", "sidebar__port--o--" <> show (num + 1) ] ++ highlight
                                       else [ "port", "sidebar__port", "sidebar__port--i", "sidebar__port--i--" <> show (num + 1) ] ++ highlight
    div_
        [ "key"       $= ( jsShow portId <> "-port-" <> jsShow num )
        , "className" $= Style.prefixFromList classes
        ] $ do
        when (isOutPort portId) $ addButton_ ref portRef
        svg_
            [ "className" $= Style.prefix "sidebar__port__svg"
            ] $ do
            circle_
                [ "className" $= Style.prefix "port__shape"
                , "key"       $= (jsShow portId <> jsShow num <> "a")
                , "fill"      $= color
                , "r"         $= jsShow2 3
                ] mempty
            when (not isOnly) $ g_ [ "className" $= Style.prefix "port__plus" ] $ do
                  plainRect_ "key1" 2 8 (-1) (-4)
                  plainRect_ "key2" 8 2 (-4) (-1)
            circle_ (
                [ "className" $= Style.prefix "port__select"
                , "key"       $= (jsShow portId <> jsShow num <> "b")
                , "r"         $= jsShow2 (lineHeight/1.5)
                ] ++ portHandlers ref mode isPortDragged isOnly portRef ) mempty

        if isInNameEditMode p then
            singleField_ [ "id" $= portLabelId ] (jsShow portId)
                $ Field.mk ref (convert $ p ^. Port.name)
                & Field.onCancel .~ Just (const $ UI.SidebarEvent $ Sidebar.PortNameDiscard portRef)
                & Field.onAccept .~ Just (UI.SidebarEvent . Sidebar.PortNameApply portRef . convert)
        else
            div_ [ "className" $= Style.prefixFromList ["sidebar__port__name", "noselect"]
                 , onDoubleClick $ \_ _ -> dispatch ref $ UI.SidebarEvent $ Sidebar.PortNameStartEdit portRef
                 ] $ elemString $ p ^. Port.name

sidebarPlaceholderForPort_ :: ReactElementM ViewEventHandler ()
sidebarPlaceholderForPort_ = div_
    [ "key"       $= "port-placeholder"
    , "className" $= Style.prefixFromList [ "port", "sidebar__port", "sidebar__port--i", "noselect" ]
    ] mempty

sidebarDraggedPort_ :: Ref App -> AnyPort -> ReactElementM ViewEventHandler ()
sidebarDraggedPort_ _ref p = withJust (getPositionInSidebar p) $ \pos ->
    div_
        [ "className" $= Style.prefixFromList [ "port", "sidebar__port", "sidebar__port--dragged", "hover" ]
        , "style"     @= Aeson.object [ "transform"  Aeson..= ("translate(0px, " <> show (pos ^. y) <> "px)") ]
        ] $ do
        div_ [ "className" $= Style.prefix "sidebar__port__name" ] $ elemString $ p ^. Port.name
        svg_
            [ "className" $= Style.prefix "sidebar__port__svg" ] $
            circle_
                [ "className" $= Style.prefix "port__shape"
                , "r"         $= jsShow2 3
                ] mempty

portLabelId :: JSString
portLabelId = Config.prefix "focus-portLabel"

focusPortLabel :: IO ()
focusPortLabel = UI.focus portLabelId
