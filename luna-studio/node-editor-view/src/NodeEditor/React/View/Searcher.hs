module NodeEditor.React.View.Searcher where

import Common.Prelude
import React.Flux

import qualified Data.Text                                 as Text
import qualified LunaStudio.Data.Searcher.Hint             as Hint
import qualified NodeEditor.Event.Keys                     as Keys
import qualified NodeEditor.Event.UI                       as UI
import qualified NodeEditor.React.Event.App                as App
import qualified NodeEditor.React.Model.NodeEditor         as NE
import qualified NodeEditor.React.Model.Searcher           as Searcher
import qualified NodeEditor.React.Model.Searcher.Mode      as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node as Node
import qualified NodeEditor.React.Model.Searcher.Mode.Node as NodeSearcher
import qualified NodeEditor.React.View.Style               as Style
import qualified React.Flux                                as React
import qualified Searcher.Data.Class                       as SearcherData
import qualified Searcher.Data.Match                       as Match

import JS.Searcher                         (searcherId)
import NodeEditor.React.Event.Searcher
import NodeEditor.React.IsRef              (IsRef, dispatch)
import NodeEditor.React.View.Visualization (docVisualization_)
import Searcher.Data.Class                 (SearcherHint)
import Searcher.Data.Result                (Result, match)
import Searcher.Data.Match                 (Range)


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

searcher :: IsRef ref => ReactView (ref, Searcher.Properties)
searcher =  React.defineView name $ \(ref, properties) -> do
    let s           = properties ^. Searcher.searcher
        mode        = s ^. Searcher.mode
        -- nodePos     = s ^. Searcher.position
        -- nodePreview = convert . (NodeLoc.empty,) <$> (s ^. Searcher.selectedNode)
        className   = "native-key-bindings " <> Style.prefixFromList ( "input" : "searcher" : ( case mode of
            Mode.Command  {} -> [ "searcher--command"]
            Mode.Node     ns -> case ns ^. Node.mode of
                Node.ExpressionMode {} -> [ "searcher--node" ]
                Node.NodeNameMode   {} -> [ "searcher--node-name"]
                Node.PortNameMode   {} -> [ "searcher--port-name"]))
        mayCustomInput = if s ^. Searcher.replaceInput 
            then ["value" $= convert (s ^. Searcher.inputText)] 
            else []
        docPresent = maybe False (not . Text.null) 
            $ s ^? Searcher.selectedResult . _Just . SearcherData.documentation
    div_
        [ "key"       $= name
        , "className" $= className
        , onMouseDown   $ \e _ -> [stopPropagation e]
        , onMouseUp     $ \e _ -> [stopPropagation e]
        , onClick       $ \e _ -> [stopPropagation e]
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $ do
        let selected = s ^. Searcher.selectedPosition
            inputClasses = Style.prefixFromList $ "searcher__input" : 
                ( if isNothing selected
                    then ["searcher__input--selected"] 
                    else [] )

        results_ ref selected $! s ^. Searcher.results
        withJust (s ^? Searcher.mode . Mode._Node 
            . Node.documentationVisualization . _Just)
                $ docVisualization_ ref docPresent
                    $ properties ^. Searcher.visualizerLibraryPath

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

searcher_ :: IsRef ref => ref -> Searcher.Properties -> ReactElementM ViewEventHandler ()
searcher_ ref model = React.viewWithSKey searcher name (ref, model) mempty

results_ :: SearcherHint a => IsRef ref => ref -> Maybe Int -> [Result a]
    -> ReactElementM ViewEventHandler ()
results_ ref selected results = if null results then return () else
    div_
        [ "key"       $= "searcherResults"
        , "className" $= Style.prefix "searcher__results"
        ] $ do
        div_
            [ "key"       $= "searcherResultsList"
            , "className" $= Style.prefix "searcher__results__list"
            ] $ forKeyed_ results $ \(idx, result) -> do
            let resultClasses i 
                    = Style.prefixFromList $ "searcher__results__item" 
                    : (if isJust selected && i == 0 
                        then [ "searcher__results__item--selected" ] 
                        else [])
            div_
                [ "key"       $= jsShow idx
                , "className" $= resultClasses idx
                , onClick     $ \e _ -> stopPropagation e : (dispatch ref $ UI.SearcherEvent $ AcceptWithHint (fromMaybe 0 selected + idx))
                ] $ do
                div_
                    ["key" $= "name"
                    ,"className" $= Style.prefix "searcher__results__item__name"
                    ] $ highlighted_ result

highlighted_ :: SearcherHint a => Result a -> ReactElementM ViewEventHandler ()
highlighted_ result = prefixElem >> highlighted_' 0 highlights where
    prefix     = convert $ result ^. SearcherData.prefix
    prefixElem = span_ [ "className" $= Style.prefix "searcher__pre"
                       , "key"       $= "searcherPre"]
                       $ elemString $ if prefix == "" then prefix else prefix <> " . "
    highlights = result ^. match . wrapped
    name'      = convert $ result ^. SearcherData.text
    highlighted_' :: Int -> [Range] -> ReactElementM ViewEventHandler ()
    highlighted_' omit [] = span_ [ "key" $= "l" ] $ elemString $ drop omit name'
    highlighted_' omit (h:rest) = do
        let start       = h ^. Match.start
            len         = h ^. Match.length
            (r1, r2)    = splitAt start name'
            normal      = drop omit r1
            highlighted = take len r2
        span_ [ "key" $= jsShow start ] $ do
            span_ [ "key" $= "n" ]
                $ elemString normal
            span_ [ "key" $= "h"
                  , "className" $= Style.prefix "searcher__hl" ]
                $ elemString highlighted
            highlighted_' (start + len) rest
