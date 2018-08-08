{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.App where

import           React.Flux                         hiding (Event)
import qualified React.Flux                         as React

import           Common.Prelude                     hiding (on)
import           Data.Timestamp                     (Timestamp (Timestamp))
import           JS.Scene                           (appId)
import qualified JS.UI                              as UI
import           NodeEditor.Event.KeyMap            (isEventHandled)
import qualified NodeEditor.Event.UI                as UI
import qualified NodeEditor.React.Event.App         as App
import qualified NodeEditor.React.Event.Breadcrumbs as Breadcrumbs
import           NodeEditor.React.IsRef             (HasApp, IsRef, dispatch)
import qualified NodeEditor.React.IsRef             as Ref
import qualified NodeEditor.React.Model.App         as App
import           NodeEditor.React.Model.Breadcrumbs (isTopLevel)
import           NodeEditor.React.View.Breadcrumbs  (breadcrumbs_)
import           NodeEditor.React.View.NodeEditor   (nodeEditor_)
import qualified NodeEditor.React.View.Style        as Style


name :: JSString
name = "app"

handleKeyDown :: IsRef ref => ref -> React.Event -> KeyboardEvent -> [SomeStoreAction]
handleKeyDown ref e k = mayStopPropagation $ dispatch ref (UI.AppEvent $ App.KeyDown k) where
    mayStopPropagation = if isEventHandled k then (preventDefault e :) else id

app :: HasApp a => ReactStore a -> ReactView ()
app ref = React.defineControllerView name ref $ \store () -> do
    let s = store ^. Ref.app
    div_
        [ onKeyDown     $ handleKeyDown ref
        , onContextMenu $ \e _ -> [preventDefault e]
        , onMouseDown   $ \e m -> dispatch ref $ UI.AppEvent $ App.MouseDown m (Timestamp (evtTimestamp e))
        , onMouseUp     $ \e m -> preventDefault e : dispatch ref (UI.AppEvent $ App.MouseUp m)
        , onMouseMove   $ \e m -> dispatch ref $ UI.AppEvent $ App.MouseMove m (Timestamp (evtTimestamp e))
        , onClick       $ \_ _ -> dispatch ref $ UI.AppEvent App.Click
        , onMouseLeave  $ \_ _ -> dispatch ref $ UI.AppEvent App.MouseLeave
        , onDoubleClick $ \_ _   -> dispatch ref $ UI.BreadcrumbsEvent Breadcrumbs.Exit
        , onWheel       $ \e m w -> preventDefault e : dispatch ref (UI.AppEvent $ App.Wheel m w)
        , onScroll      $ \e     -> [preventDefault e]
        , "key"       $= "app"
        , "id"        $= appId
        , "tabIndex"  $= "-1"
        , "className" $= Style.prefixFromList [ "studio", "noselect"]
        ] $ do
        nodeEditor_  ref (s ^. App.nodeEditor) (isTopLevel $ s ^. App.breadcrumbs)
        breadcrumbs_ ref (s ^. App.moduleName) $ s ^. App.breadcrumbs
