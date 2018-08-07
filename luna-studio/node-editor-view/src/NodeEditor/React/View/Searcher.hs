{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Searcher where

import           Common.Prelude
import qualified Data.Text                                 as Text
import           JS.Searcher                               (searcherId)
import           LunaStudio.Data.NodeSearcher              (Match, Range)
import qualified LunaStudio.Data.NodeSearcher              as NS
import qualified NodeEditor.Event.Keys                     as Keys
import qualified NodeEditor.Event.UI                       as UI
import qualified NodeEditor.React.Event.App                as App
import           NodeEditor.React.Event.Searcher
import           NodeEditor.React.IsRef                    (IsRef, dispatch)
import           NodeEditor.React.Model.SearcherProperties (SearcherProperties)
import qualified NodeEditor.React.Model.SearcherProperties as Searcher
import qualified NodeEditor.React.View.Style               as Style
import           NodeEditor.React.View.Visualization       (docVisualization_)
import           React.Flux
import qualified React.Flux                                as React


name :: JSString
name = "searcher"

handleKeyDown :: IsRef ref => ref -> React.Event -> KeyboardEvent -> [SomeStoreAction]
handleKeyDown ref e k = prevent $ stopPropagation e : dispatch' where
    prevent   = if Keys.withoutMods k Keys.tab
                || Keys.withoutMods k Keys.upArrow
                || Keys.withoutMods k Keys.downArrow
                || Keys.digitWithCtrl k then (preventDefault e :) else id
    dispatch' = dispatch ref $ if Keys.withoutMods k Keys.esc then
            UI.AppEvent $ App.KeyDown k
        else UI.SearcherEvent $ KeyDown k

searcher :: IsRef ref => ReactView (ref, SearcherProperties)
searcher =  React.defineView name $ \(ref, s) -> do
    let mode        = s ^. Searcher.mode
        -- nodePos     = s ^. Searcher.position
        -- nodePreview = convert . (NodeLoc.empty,) <$> (s ^. Searcher.selectedNode)
        className   = "native-key-bindings " <> Style.prefixFromList ( "input" : "searcher" : ( case mode of
            Searcher.Command  {} -> [ "searcher--command"]
            Searcher.Node     {} -> [ "searcher--node" ]
            Searcher.NodeName {} -> [ "searcher--node-name"]
            Searcher.PortName {} -> [ "searcher--port-name"]))
        mayCustomInput = if s ^. Searcher.replaceInput then ["value" $= convert (s ^. Searcher.input)] else []
        docPresent = maybe False (not . Text.null) $ s ^? Searcher.selectedMatch . _Just . NS.doc
    div_
        [ "key"       $= name
        , "className" $= className
        , onMouseDown   $ \e _ -> [stopPropagation e]
        , onMouseUp     $ \e _ -> [stopPropagation e]
        , onClick       $ \e _ -> [stopPropagation e]
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $ do
        let inputClasses = Style.prefixFromList $ "searcher__input" : (if s ^. Searcher.selected == 0 then ["searcher__input--selected"] else [])
            selected     = s ^. Searcher.selected

        -- TODO [LJK, PM]: Refactor this piece of code:
        case s ^. Searcher.mode of
            Searcher.Command    results -> do results_ ref selected results
            Searcher.Node _ nmi results -> do results_ ref selected results
                                              withJust (s ^. Searcher.docVis) $ docVisualization_ ref docPresent (s ^. Searcher.visLibPath)
            Searcher.NodeName _ results -> do results_ ref selected results
            Searcher.PortName _ results -> do results_ ref selected results

        input_ (
            [ "key"         $= "searchInput"
            , "className"   $= inputClasses
            , "id"          $= searcherId
            , onKeyDown     $ handleKeyDown ref
            , onKeyUp       $ \_ k -> dispatch ref $ UI.SearcherEvent $ KeyUp k
            , onChange      $ \e -> let val = target e "value"
                                        ss  = target e "selectionStart"
                                        se  = target e "selectionEnd"
                                    in dispatch ref $ UI.SearcherEvent $ InputChanged val ss se
            ] <> mayCustomInput )

    -- div_
        --     [ "key"       $= "searcherPreview"
        --     , "className" $= Style.prefix "searcher__preview"
        --     ] $ withJust nodePreview $ nodeBody_ ref . (Node.position .~ nodePos)
                                              -- . (Node.isExpandedControls .~ True)

searcher_ :: IsRef ref => ref -> SearcherProperties -> ReactElementM ViewEventHandler ()
searcher_ ref model = React.viewWithSKey searcher name (ref, model) mempty

results_ :: IsRef ref => ref -> Int -> [Match] -> ReactElementM ViewEventHandler ()
results_ ref selected results = if results == [] then return () else
    div_
        [ "key"       $= "searcherResults"
        , "className" $= Style.prefix "searcher__results"
        ] $ do
        div_
            [ "key"       $= "searcherResultsList"
            , "className" $= Style.prefix "searcher__results__list"
            ] $ forKeyed_ (take 10 $ drop (selected - 1) results) $ \(idx, result) -> do
            let resultClasses i = Style.prefixFromList $ "searcher__results__item" : (if selected > 0 && i == 0 then [ "searcher__results__item--selected" ] else [])
            div_
                [ "key"       $= jsShow idx
                , "className" $= resultClasses idx
                , onClick     $ \e _ -> stopPropagation e : (dispatch ref $ UI.SearcherEvent $ AcceptWithHint (idx + 1))
                ] $ do
                div_
                    ["key" $= "name"
                    ,"className" $= Style.prefix "searcher__results__item__name"
                    ] $ highlighted_ result

highlighted_ :: Match -> ReactElementM ViewEventHandler ()
highlighted_ result = prefixElem >> highlighted_' 0 highlights where
    prefix     = convert $ result ^. NS.className
    prefixElem = span_ [ "className" $= Style.prefix "searcher__pre"
                       , "key"       $= "searcherPre"]
                       $ elemString $ if prefix == "" then prefix else prefix <> " . "
    highlights = result ^. NS.match
    name'      = convert $ result ^. NS.name
    highlighted_' :: Int -> [Range] -> ReactElementM ViewEventHandler ()
    highlighted_' omit [] = span_ [ "key" $= "l" ] $ elemString $ snd $ splitAt omit name'
    highlighted_' omit ((start, end):rest) = do
        let len                   = end - start
            (r1         , r2    ) = splitAt start name'
            (_          , normal) = splitAt omit r1
            (highlighted, _     ) = splitAt len r2
        span_ [ "key" $= jsShow start ] $ do
            span_ [ "key" $= "n" ]
                $ elemString normal
            span_ [ "key" $= "h"
                  , "className" $= Style.prefix "searcher__hl" ]
                $ elemString highlighted
            highlighted_' (start + len) rest
