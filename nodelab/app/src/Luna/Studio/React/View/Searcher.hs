
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Searcher where

import qualified Data.Aeson                       as Aeson
import           Data.Matrix                      (Matrix)
import           Empire.API.Data.Node             (NodeType (ExpressionNode))
import qualified Empire.API.Data.Node             as Empire
import           JS.Searcher                      (searcherId)
import           Luna.Studio.Data.Matrix          (showNodeTranslate)
import qualified Luna.Studio.Event.Keys           as Keys
import qualified Luna.Studio.Event.UI             as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App      as App
import           Luna.Studio.React.Event.Searcher
import           Luna.Studio.React.Model.App      (App)
import qualified Luna.Studio.React.Model.Node     as Node
import           Luna.Studio.React.Model.Searcher (Searcher)
import qualified Luna.Studio.React.Model.Searcher as Searcher
import           Luna.Studio.React.Store          (Ref, dispatch)
import           Luna.Studio.React.View.Node      (nodeBody_)
import qualified Luna.Studio.React.View.Style     as Style
import           React.Flux
import qualified React.Flux                       as React
import qualified Text.ScopeSearcher.QueryResult   as Result


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
    let nodePos   = s ^. Searcher.position
        mode      = s ^. Searcher.mode
        nodePrev  = flip (maybe Nothing) (s ^. Searcher.selectedNode) $
            \empireNode -> case empireNode ^. Empire.nodeType of
                ExpressionNode expr -> Just $ convert (empireNode, expr)
                _                   -> Nothing
        className = Style.prefixFromList ( "node-root" : (case mode of
                                                          Searcher.Node    { } -> [ "searcher" ]
                                                          Searcher.Command { } -> [ "searcher", "searcher--command" ]))
    div_
        [ "key"       $= name
        , "className" $= className
        ] $ do
        div_
            [ "key"       $= "nodeTrans"
            , "className" $= Style.prefix "node-trans"
            ] $ withJust nodePrev $ nodeBody_ ref . (Node.position .~ nodePos) -- . (Node.isExpandedControls .~ True)
        div_
            [ "key"       $= "nameTrans"
            , "className" $= Style.prefix "name-trans"
            , "style"     @= Aeson.object [ "transform" Aeson..= (showNodeTranslate camera $ s ^. Searcher.position) ]
            , onMouseDown $ \e _ -> [stopPropagation e]
            , onMouseUp   $ \e _ -> [stopPropagation e]
            , onClick     $ \e _ -> [stopPropagation e]
            ] $ do
            input_
                [ "key"       $= "searchInput"
                , "className" $= Style.prefix "searcher__input"
                , "id"        $= searcherId
                , "value"     $= convert (s ^. Searcher.input)
                , onKeyDown   $ handleKeyDown ref
                , onKeyUp     $ \_ k -> dispatch ref $ UI.SearcherEvent $ KeyUp k
                , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.SearcherEvent $ InputChanged val
                ]
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

searcher_ :: Ref App -> Matrix Double -> Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref camera model = React.viewWithSKey searcher name (ref, camera, model) mempty
