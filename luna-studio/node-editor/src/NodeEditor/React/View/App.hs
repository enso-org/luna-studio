{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.App where

import           React.Flux                             hiding (Event)
import qualified React.Flux                             as React

import           Common.Prelude                         hiding (on)
import           Data.Timestamp                         (Timestamp (Timestamp))
import           JS.Scene                               (appId)
import qualified JS.UI                                  as UI
import qualified NodeEditor.Event.Event                 as Event
import           NodeEditor.Event.Preprocessor.Shortcut (isEventHandled)
import qualified NodeEditor.Event.Shortcut              as Shortcut
import qualified NodeEditor.Event.UI                    as UI
import qualified NodeEditor.React.Event.App             as App
import           NodeEditor.React.Model.App             (App)
import qualified NodeEditor.React.Model.App             as App
import           NodeEditor.React.Store                 (Ref, dispatch, dispatch', dt)
import           NodeEditor.React.View.Breadcrumbs      (breadcrumbs_)
import           NodeEditor.React.View.NodeEditor       (nodeEditor_)
import qualified NodeEditor.React.View.Style            as Style


name :: JSString
name = "app"

handleKeyDown :: Ref App -> React.Event -> KeyboardEvent -> [SomeStoreAction]
handleKeyDown ref e k = mayStopPropagation $ dispatch ref (UI.AppEvent $ App.KeyDown k) where
    mayStopPropagation = if isEventHandled k then (preventDefault e :) else id

app :: Ref App -> ReactView ()
app ref = React.defineControllerView name ref $ \store () -> do
    let s = store ^. dt
    div_
        [ onKeyDown     $ handleKeyDown ref
        , onContextMenu $ \e _ -> [preventDefault e]
        , onMouseDown   $ \e m -> dispatch ref $ UI.AppEvent $ App.MouseDown m (Timestamp (evtTimestamp e))
        , onMouseUp     $ \e m -> preventDefault e : dispatch ref (UI.AppEvent $ App.MouseUp m)
        , onMouseMove   $ \e m -> dispatch ref $ UI.AppEvent $ App.MouseMove m (Timestamp (evtTimestamp e))
        , onClick       $ \_ _ -> dispatch ref $ UI.AppEvent App.Click
        , onMouseLeave  $ \_ _ -> dispatch ref $ UI.AppEvent App.MouseLeave
        , onDoubleClick $ \_ _   -> dispatch' ref $ Event.Shortcut $ Shortcut.Event Shortcut.ExitGraph def
        , onWheel       $ \e m w -> preventDefault e : dispatch ref (UI.AppEvent $ App.Wheel m w)
        , onScroll      $ \e     -> [preventDefault e]
        -- , on "onPaste"  $ \e   -> let val = Clipboard.getClipboardData (evtHandlerArg e)
        --                           in dispatch' ref $ Shortcut $ Shortcut.Event Shortcut.Paste $ Just val
        -- , on "onCut"    $ \_   -> dispatch' ref $ Shortcut $ Shortcut.Event Shortcut.Cut def
        -- , on "onCopy"   $ \_   -> dispatch' ref $ Shortcut $ Shortcut.Event Shortcut.Copy def
        , "key"       $= "app"
        , "id"        $= appId
        , "tabIndex"  $= "-1"
        , "className" $= Style.prefixFromList [ "studio", "noselect"]
        ] $ do
        nodeEditor_  ref $ s ^. App.nodeEditor
        breadcrumbs_ ref $ s ^. App.breadcrumbs

focus :: MonadIO m => m ()
focus = UI.focus appId
