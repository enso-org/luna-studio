{-# LANGUAGE OverloadedStrings #-}
module Node.Editor.React.View.Searcher where

import qualified Data.Aeson                                  as Aeson
import           Data.Matrix                                 (Matrix)
import qualified Empire.API.Data.NodeLoc                     as NodeLoc
import           JS.Searcher                                 (searcherId)
import           Node.Editor.Data.Matrix                     (showNodeTranslate)
import qualified Node.Editor.Event.Keys                      as Keys
import qualified Node.Editor.Event.UI                        as UI
import           Luna.Prelude
import qualified Node.Editor.React.Event.App                 as App
import           Node.Editor.React.Event.Searcher
import           Node.Editor.React.Model.App                 (App)
import qualified Node.Editor.React.Model.Node.ExpressionNode as Node
import           Node.Editor.React.Model.Searcher            (Searcher)
import qualified Node.Editor.React.Model.Searcher            as Searcher
import           Node.Editor.React.Store                     (Ref, dispatch)
import           Node.Editor.React.View.ExpressionNode       (nodeBody_)
import qualified Node.Editor.React.View.Style                as Style
import           React.Flux
import qualified React.Flux                                  as React
import qualified Text.ScopeSearcher.QueryResult              as Result

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

searcher :: ReactView (Ref App, Matrix Double, Searcher)
searcher =  React.defineView name $ \(ref, camera, s) -> do
    let nodePos     = s ^. Searcher.position
        mode        = s ^. Searcher.mode
        nodePreview = convert . (NodeLoc.empty,) <$> (s ^. Searcher.selectedNode)
        className   = Style.prefixFromList ( "searcher" : ( case mode of
                                                                    Searcher.Node    { } -> [ "searcher--node" ]
                                                                    Searcher.Command { } -> [ "searcher--command"   ]))
        mayCustomInput = if s ^. Searcher.replaceInput then ["value" $= convert (s ^. Searcher.input)] else []
    div_
        [ "key"       $= name
        , "className" $= className
        ] $ do
        div_
            [ "key"       $= "searcherBody"
            , "className" $= Style.prefix "searcher__body"
            , "style"     @= Aeson.object [ "transform" Aeson..= (showNodeTranslate camera $ s ^. Searcher.position) ]
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
                    Searcher.Node results -> forKeyed_ results $ \(idx, result) ->
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
        div_
            [ "key"       $= "searcherPreview"
            , "className" $= Style.prefix "searcher__preview"
            ] $ withJust nodePreview $ nodeBody_ ref . (Node.position .~ nodePos)
                                              -- . (Node.isExpandedControls .~ True)

searcher_ :: Ref App -> Matrix Double -> Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref camera model = React.viewWithSKey searcher name (ref, camera, model) mempty
