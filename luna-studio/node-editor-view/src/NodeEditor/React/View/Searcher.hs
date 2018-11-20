module NodeEditor.React.View.Searcher where

import Common.Prelude
import React.Flux

import qualified Data.Text                                 as Text
import qualified NodeEditor.Event.Keys                     as Keys
import qualified NodeEditor.Event.UI                       as UI
import qualified NodeEditor.React.Event.App                as App
import qualified NodeEditor.React.Model.NodeEditor         as NE
import qualified NodeEditor.React.Model.Searcher.Mode.Node as NodeSearcher
import qualified NodeEditor.React.Model.SearcherProperties as Searcher
import qualified NodeEditor.React.View.Style               as Style
import qualified React.Flux                                as React
import qualified Searcher.Engine.Data.Match                as Match
import qualified Searcher.Engine.Data.Symbol               as Symbol

import JS.Searcher                               (searcherId)
import NodeEditor.React.Event.Searcher
import NodeEditor.React.IsRef                    (IsRef, dispatch)
import NodeEditor.React.Model.SearcherProperties (Match, Range, SearcherData,
                                                  SearcherProperties)
import NodeEditor.React.View.Visualization       (docVisualization_)


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
            Searcher.CommandSearcher  {} -> [ "searcher--command"]
            Searcher.NodeSearcher     ns -> case ns ^. Searcher.modeData of
                Searcher.ExpressionMode {} -> [ "searcher--node" ]
                Searcher.NodeNameMode   {} -> [ "searcher--node-name"]
                Searcher.PortNameMode   {} -> [ "searcher--port-name"]))
        mayCustomInput = if s ^. Searcher.replaceInput then ["value" $= convert (s ^. Searcher.input)] else []
        docPresent = maybe False (not . Text.null) $ s ^? Searcher.selectedHint . _Just . Searcher.documentation . _Just
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
            Searcher.CommandSearcher results -> do results_ ref selected results
            Searcher.NodeSearcher ns -> let results = ns ^. NodeSearcher.nodes
                in case ns ^. Searcher.modeData of
                    Searcher.ExpressionMode {} -> do
                        results_ ref selected results
                        withJust (s ^. Searcher.documentationVisualization) $
                            docVisualization_ ref docPresent (s ^. Searcher.visualizerLibraryPath)

                    Searcher.NodeNameMode {} -> do results_ ref selected results
                    Searcher.PortNameMode {} -> do results_ ref selected results

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

results_ :: SearcherData a => IsRef ref => ref -> Int -> [Match a] -> ReactElementM ViewEventHandler ()
results_ ref selected results = if null results then return () else
    div_
        [ "key"       $= "searcherResults"
        , "className" $= Style.prefix "searcher__results"
        ] $ do
        div_
            [ "key"       $= "searcherResultsList"
            , "className" $= Style.prefix "searcher__results__list"
            ] $ forKeyed_ results $ \(idx, result) -> do
            let resultClasses i = Style.prefixFromList $ "searcher__results__item" : (if selected > 0 && i == 0 then [ "searcher__results__item--selected" ] else [])
            div_
                [ "key"       $= jsShow idx
                , "className" $= resultClasses idx
                , onClick     $ \e _ -> stopPropagation e : (dispatch ref $ UI.SearcherEvent $ AcceptWithHint (selected + idx))
                ] $ do
                div_
                    ["key" $= "name"
                    ,"className" $= Style.prefix "searcher__results__item__name"
                    ] $ highlighted_ result

highlighted_ :: SearcherData a => Match a -> ReactElementM ViewEventHandler ()
highlighted_ result = prefixElem >> highlighted_' 0 highlights where
    prefix     = convert $ result ^. Match.prefix
    prefixElem = span_ [ "className" $= Style.prefix "searcher__pre"
                       , "key"       $= "searcherPre"]
                       $ elemString $ if prefix == "" then prefix else prefix <> " . "
    highlights = result ^. Searcher.matchedCharacters
    name'      = convert $ result ^. Searcher.name
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
