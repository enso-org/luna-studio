{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.App where

import           React.Flux                              hiding (Event)
import qualified React.Flux                              as React
import           React.Flux.Internal                     (HandlerArg (HandlerArg))
import           System.IO.Unsafe                        (unsafePerformIO)

import           Luna.Studio.Event.Event                 (Event (Shortcut))
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

app :: Ref App -> ReactView ()
app ref = React.defineControllerView name ref $ \store () -> do
    let s = store ^. dt
    div_
        [ onKeyDown     $ \e k -> dispatch ref (UI.AppEvent $ App.KeyDown k)
        , onContextMenu $ \e _ -> [preventDefault e]
        , onMouseDown   $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseDown m
        , onMouseUp     $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseUp   m
        , onMouseMove   $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseMove m
        , onClick       $ \_ m -> dispatch ref $ UI.AppEvent $ App.Click     m
        , on "onPaste"  $ \e   -> let val = getClipboardData (evtHandlerArg e)
                                  in dispatch' ref $ Shortcut $ Shortcut.Paste val
        , on "onCut"    $ \e   -> dispatch' ref $ Shortcut   Shortcut.Cut
        , on "onCopy"   $ \e   -> dispatch' ref $ Shortcut   Shortcut.Copy
        , "key"       $= "app"
        , "id"        $= "focus-root"
        , "tabIndex"  $= "-1"
        , "className" $= "luna-studio"
        ] $ do
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

foreign import javascript safe "document.getElementById('focus-root').focus()" focus :: IO ()

foreign import javascript safe "$1.clipboardData.getData('Text')" getClipboardData' :: HandlerArg -> IO JSString

getClipboardData :: HandlerArg -> Text
getClipboardData = unsafePerformIO . fmap convert . getClipboardData'
