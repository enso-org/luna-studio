{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Searcher where

import qualified Data.Aeson                       as Aeson
import           Data.Position                    (Position (Position), Vector2 (Vector2))
import           Data.Vector
import           React.Flux
import qualified React.Flux                       as React

import qualified JS.Config                        as Config
import qualified JS.UI                            as UI
import qualified Luna.Studio.Event.Keys           as Keys
import qualified Luna.Studio.Event.UI             as UI
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Searcher
import           Luna.Studio.React.Model.App      (App)
import qualified Luna.Studio.React.Model.Node     as Node
import           Luna.Studio.React.Model.Searcher (Searcher)
import qualified Luna.Studio.React.Model.Searcher as Searcher
import           Luna.Studio.React.Store          (Ref, dispatch)
import           Luna.Studio.React.View.NodeBody  (nodeBody_)
import qualified Text.ScopeSearcher.QueryResult   as Result

name :: JSString
name = "searcher"

preventTabDefault :: React.Event -> KeyboardEvent -> [SomeStoreAction] -> [SomeStoreAction]
preventTabDefault e k r = if Keys.withoutMods k Keys.tab then preventDefault e : r else r

searcher :: ReactView (Ref App, Searcher)
searcher  = React.defineView name $ \(ref, s) -> do
    let pos  = s ^. Searcher.position
        widgetPos = pos -- - Position (Vector2 50 50)
        nodePos   = Position (Vector2 0 0)
        mode = s ^. Searcher.mode
        nodePreview = Node.fromNode <$> s ^. Searcher.selectedNode
        className = "luna-" <> name <> " luna-" <> case mode of
            Searcher.Node    { } -> "node-" <> name
            Searcher.Command { } -> "command-" <> name
    div_
        [ "key"       $= name
        , "className" $= className
        , "style"     @= Aeson.object
            [ "top"  Aeson..= (show (widgetPos ^. y) <> "px" :: String)
            , "left" Aeson..= (show (widgetPos ^. x) <> "px" :: String)
            ]
        ] $ do
        withJust nodePreview $ nodeBody_ ref . (Node.position .~ nodePos)
                                            --  . (Node.isExpanded .~ True)
        input_
            [ "key"   $= "input"
            , "id"    $= searcherId
            , "value" $= convert (s ^. Searcher.input)
            , onMouseDown $ \e _ -> [stopPropagation e]
            , onKeyDown   $ \e k -> preventTabDefault e k $ stopPropagation e : dispatch ref (UI.SearcherEvent $ KeyDown k)
            , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.SearcherEvent $ InputChanged val
            ]
        div_
            [ "key"       $= "results"
            , "className" $= "luna-searcher-results"
            ] $
            case s ^. Searcher.mode of
                Searcher.Command results -> forKeyed_ results $ \(idx, result) ->
                    div_
                        [ "key"       $= jsShow idx
                        , "className" $= if idx == s ^. Searcher.selected then "luna-result-selected" else "luna-result"
                        ] $
                        div_
                            ["key" $= "name"
                            ,"className" $= "luna-result-name"
                            ] $ elemString $ convert $ result ^. Result.name
                Searcher.Node results -> forKeyed_ results $ \(idx, result) ->
                    div_
                        [ "key"       $= jsShow idx
                        , "className" $= if idx == s ^. Searcher.selected then "luna-result-selected" else "luna-result"
                        ] $ do
                        div_
                            ["key"       $= "prefix"
                            ,"className" $= "luna-result-prefix"
                            ] $ elemString $ convert (result ^. Result.prefix) <> "."
                        div_
                            ["key" $= "name"
                            ,"className" $= "luna-result-name"
                            ] $ elemString $ convert $ result ^. Result.name

searcher_ :: Ref App -> Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref model = React.viewWithSKey searcher name (ref, model) mempty

searcherId :: JSString
searcherId = Config.prefix "focus-searcher"

selection :: MonadIO m => m (Int, Int)
selection = liftIO $ (,) <$> selectionStart searcherId <*> selectionEnd searcherId

foreign import javascript safe "document.getElementById($1).selectionStart" selectionStart :: JSString -> IO Int
foreign import javascript safe "document.getElementById($1).selectionEnd"   selectionEnd   :: JSString -> IO Int

focus :: IO ()
focus = UI.focus searcherId
