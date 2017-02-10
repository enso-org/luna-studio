{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.App where

import           React.Flux                              hiding (Event)
import qualified React.Flux                              as React

import           Data.Timestamp                          (Timestamp (Timestamp))
import qualified JS.Clipboard                            as Clipboard
import           JS.Scene                                (appId)
import qualified JS.UI                                   as UI
import           Luna.Studio.Event.Event                 (Event (Shortcut))
import           Luna.Studio.Event.Preprocessor.Shortcut (isEventHandled)
import qualified Luna.Studio.Event.Shortcut              as Shortcut
import qualified Luna.Studio.Event.UI                    as UI
import           Luna.Studio.Prelude                     hiding (on)
import qualified Luna.Studio.React.Event.App             as App
import           Luna.Studio.React.Model.App             (App)
import qualified Luna.Studio.React.Model.App             as App
import           Luna.Studio.React.Store                 (Ref, dispatch, dispatch', dt)
import           Luna.Studio.React.View.Breadcrumbs      (breadcrumbs_)
import           Luna.Studio.React.View.CodeEditor       (codeEditor_)
import           Luna.Studio.React.View.CodeEditorToggle (codeEditorToggle_)
import           Luna.Studio.React.View.NodeEditor       (nodeEditor_)
import           Luna.Studio.React.View.Searcher         (searcher_)


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
        , onMouseUp     $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseUp   m
        , onMouseMove   $ \e m -> dispatch ref $ UI.AppEvent $ App.MouseMove m (Timestamp (evtTimestamp e))
        , onClick       $ \_ _ -> dispatch ref $ UI.AppEvent $ App.Click
        , onMouseLeave  $ \_ _ -> dispatch ref $ UI.AppEvent   App.MouseLeave
        , on "onPaste"  $ \e   -> let val = Clipboard.getClipboardData (evtHandlerArg e)
                                  in dispatch' ref $ Shortcut $ Shortcut.Paste val
        , on "onCut"    $ \_   -> dispatch' ref $ Shortcut   Shortcut.Cut
        , on "onCopy"   $ \_   -> dispatch' ref $ Shortcut   Shortcut.Copy
        , "key"       $= "app"
        , "id"        $= appId
        , "tabIndex"  $= "-1"
        , "className" $= "luna-studio"
        ] $
        div_
            [ "className" $= "luna-main"
            , "key"       $= "main"
            ] $ do
            div_
                [ "className" $= "luna-graph-editor"
                , "key"       $= "graph-editor"
                ] $ do
                nodeEditor_  ref $ s ^. App.nodeEditor
                breadcrumbs_ ref $ s ^. App.breadcrumbs
                codeEditorToggle_ ref
                mapM_ (searcher_ ref) $ s ^. App.searcher
            codeEditor_ $ s ^. App.codeEditor

focus :: IO ()
focus = UI.focus appId
