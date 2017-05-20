{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Searcher where

import           Common.Prelude
import           JS.Searcher                     (searcherId)
import qualified NodeEditor.Event.Keys           as Keys
import qualified NodeEditor.Event.UI             as UI
import qualified NodeEditor.React.Event.App      as App
import           NodeEditor.React.Event.Searcher
import           NodeEditor.React.Model.App      (App)
import           NodeEditor.React.Model.Searcher (Searcher)
import qualified NodeEditor.React.Model.Searcher as Searcher
import           NodeEditor.React.Store          (Ref, dispatch)
import qualified NodeEditor.React.View.Style     as Style
import           React.Flux
import qualified React.Flux                      as React
import qualified Text.ScopeSearcher.QueryResult  as Result

name :: JSString
name = "searcher"

handleKeyDown :: Ref App -> React.Event -> KeyboardEvent -> [SomeStoreAction]
handleKeyDown ref e k = prevent $ stopPropagation e : dispatch' where
    prevent   = if Keys.withoutMods k Keys.tab
                || Keys.withoutMods k Keys.upArrow
                || Keys.withoutMods k Keys.downArrow
                || Keys.digitWithCtrl k then (preventDefault e :) else id
    dispatch' = dispatch ref $ if Keys.withoutMods k Keys.esc then
            UI.AppEvent $ App.KeyDown k
        else UI.SearcherEvent $ KeyDown k

searcher :: ReactView (Ref App, Searcher)
searcher =  React.defineView name $ \(ref, s) -> do
    let mode        = s ^. Searcher.mode
        -- nodePos     = s ^. Searcher.position
        -- nodePreview = convert . (NodeLoc.empty,) <$> (s ^. Searcher.selectedNode)
        className   = Style.prefixFromList ( "input" : "searcher" : ( case mode of
            Searcher.Command      _ -> [ "searcher--command"]
            Searcher.Node     _ _ _ -> [ "searcher--node" ]
            Searcher.NodeName _   _ -> [ "searcher--node-name"]
            Searcher.PortName _   _ -> [ "searcher--port-name"]))
        mayCustomInput = if s ^. Searcher.replaceInput then ["value" $= convert (s ^. Searcher.input)] else []
    div_
        [ "key"       $= name
        , "className" $= className
        ] $ do
        div_
            [ "key"       $= "searcherBody"
            , "className" $= Style.prefix "searcher__body"
            -- , "style"     @= Aeson.object [ "transform" Aeson..= (showNodeTranslate camera $ s ^. Searcher.position) ]
            , onMouseDown $ \e _ -> [stopPropagation e]
            , onMouseUp   $ \e _ -> [stopPropagation e]
            , onClick     $ \e _ -> [stopPropagation e]
            ] $ do
            input_ (
                [ "key"       $= "searchInput"
                , "className" $= Style.prefix "searcher__input"
                , "id"        $= searcherId
                , onKeyDown   $ handleKeyDown ref
                , onKeyUp     $ \_ k -> dispatch ref $ UI.SearcherEvent $ KeyUp k
                , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.SearcherEvent $ InputChanged val
                ] ++ mayCustomInput )
            div_
                [ "key"       $= "searcherResults"
                , "className" $= Style.prefix "searcher__results"
                ] $ do
                let resultClasses i = Style.prefixFromList ( "searcher__results__item" : (if i + 1 == s ^. Searcher.selected then [ "searcher__results__item--selected" ] else []))
                -- TODO [LJK, PM]: Refactor this piece of code:
                case s ^. Searcher.mode of
                    Searcher.Command results -> forKeyed_ results $ \(idx, result) ->
                        div_
                            [ "key"       $= jsShow idx
                            , "className" $= resultClasses idx
                            , onClick     $ \e _ -> stopPropagation e : (dispatch ref $ UI.SearcherEvent $ AcceptEntry (idx + 1))
                            ] $
                            div_
                                [ "key" $= "name"
                                , "className" $= Style.prefix "searcher__result__item__name"
                                ] $ elemString $ convert $ result ^. Result.name
                    Searcher.Node _ _ results -> forKeyed_ results $ \(idx, result) ->
                        div_
                            [ "key"       $= jsShow idx
                            , "className" $= resultClasses idx
                            , onClick     $ \e _ -> stopPropagation e : (dispatch ref $ UI.SearcherEvent $ AcceptEntry (idx + 1))
                            ] $ do
                            div_
                                ["key"       $= "prefix"
                                ,"className" $= Style.prefix "searcher__results__item__prefix"
                                ] $ elemString $ convert (result ^. Result.prefix) <> "."
                            div_
                                ["key" $= "name"
                                ,"className" $= Style.prefix "searcher__results__item__name"
                                ] $ elemString $ convert $ result ^. Result.name
                    Searcher.NodeName _ results -> forKeyed_ results $ \(idx, result) ->
                        div_
                            [ "key"       $= jsShow idx
                            , "className" $= resultClasses idx
                            , onClick     $ \e _ -> stopPropagation e : (dispatch ref $ UI.SearcherEvent $ AcceptEntry (idx + 1))
                            ] $ do
                            div_
                                ["key"       $= "prefix"
                                ,"className" $= Style.prefix "searcher__results__item__prefix"
                                ] $ elemString $ convert (result ^. Result.prefix) <> "."
                            div_
                                ["key" $= "name"
                                ,"className" $= Style.prefix "searcher__results__item__name"
                                ] $ elemString $ convert $ result ^. Result.name
                    Searcher.PortName _ results -> forKeyed_ results $ \(idx, result) ->
                        div_
                            [ "key"       $= jsShow idx
                            , "className" $= resultClasses idx
                            , onClick     $ \e _ -> stopPropagation e : (dispatch ref $ UI.SearcherEvent $ AcceptEntry (idx + 1))
                            ] $ do
                            div_
                                ["key"       $= "prefix"
                                ,"className" $= Style.prefix "searcher__results__item__prefix"
                                ] $ elemString $ convert (result ^. Result.prefix) <> "."
                            div_
                                ["key" $= "name"
                                ,"className" $= Style.prefix "searcher__results__item__name"
                                ] $ elemString $ convert $ result ^. Result.name
        -- div_
        --     [ "key"       $= "searcherPreview"
        --     , "className" $= Style.prefix "searcher__preview"
        --     ] $ withJust nodePreview $ nodeBody_ ref . (Node.position .~ nodePos)
                                              -- . (Node.isExpandedControls .~ True)

searcher_ :: Ref App -> Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref model = React.viewWithSKey searcher name (ref, model) mempty
