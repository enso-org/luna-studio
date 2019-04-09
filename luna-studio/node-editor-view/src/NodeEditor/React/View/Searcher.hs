module NodeEditor.React.View.Searcher where

import Common.Prelude
import React.Flux

import qualified Data.Text                                 as Text
import qualified NodeEditor.Event.Keys                     as Keys
import qualified NodeEditor.Event.UI                       as UI
import qualified NodeEditor.React.Event.App                as App
import qualified NodeEditor.React.Model.Searcher           as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint      as Hint
import qualified NodeEditor.React.Model.Searcher.Hint.Node as NodeHint
import qualified NodeEditor.React.Model.Searcher.Mode      as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node as Node
import qualified NodeEditor.React.View.Style               as Style
import qualified React.Flux                                as React
import qualified Searcher.Data.Class                       as SearcherData
import qualified Searcher.Data.Match                       as Match
import qualified Searcher.Data.Result                      as Result

import JS.Searcher                          (searcherId)
import NodeEditor.React.Event.Searcher
import NodeEditor.React.IsRef               (IsRef, dispatch)
import NodeEditor.React.Model.Searcher.Hint (Hint)
import NodeEditor.React.View.Visualization  (docVisualization_)
import Searcher.Data.Match                  (Range)
import Searcher.Data.Result                 (Result)


name :: JSString
name = "searcher"

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
            mayDocVis = s ^? Searcher.mode . Mode._Node
                           . Node.documentationVisualization . _Just
            visualizersPath = properties ^. Searcher.visualizerLibraryPath

        results_ ref selected (s ^. Searcher.waiting) (s ^. Searcher.results)

        withJust mayDocVis $ docVisualization_ ref docPresent visualizersPath

        input_ (
            [ "key"         $= "searchInput"
            , "className"   $= inputClasses
            , "id"          $= searcherId
            , onKeyUp       $ \_ k -> dispatch ref $ UI.SearcherEvent $ KeyUp k
            , onChange      $ \e -> let val = target e "value"
                                        ss  = target e "selectionStart"
                                        se  = target e "selectionEnd"
                                    in dispatch ref $ UI.SearcherEvent $ InputChanged val ss se
            , onClick       $ \_ _ -> dispatch ref $ UI.SearcherEvent SelectionChanged
            ] <> mayCustomInput )

    -- div_
        --     [ "key"       $= "searcherPreview"
        --     , "className" $= Style.prefix "searcher__preview"
        --     ] $ withJust nodePreview $ nodeBody_ ref . (Node.position .~ nodePos)
                                              -- . (Node.isExpandedControls .~ True)

searcher_ :: IsRef ref => ref -> Searcher.Properties -> ReactElementM ViewEventHandler ()
searcher_ ref model = React.viewWithSKey searcher name (ref, model) mempty

results_ :: IsRef ref => ref -> Maybe Int -> Bool -> [Result Hint]
    -> ReactElementM ViewEventHandler ()
results_ ref selected wait results = when (not (null results) || wait) $ do
    div_
        [ "key"       $= "searcherResults"
        , "className" $= Style.prefix "searcher__results"
        ] $ do
        div_
            [ "key"       $= "searcherResultsList"
            , "className" $= Style.prefix "searcher__results__list"
            ] $ do
            forKeyed_ results $ \(idx, result) -> do
                let resultClasses i
                        = Style.prefixFromList $ "searcher__results__item"
                        : (if isJust selected && i == 0
                            then [ "searcher__results__item--selected" ]
                            else [])
                div_
                    [ "key"       $= jsShow idx
                    , "className" $= resultClasses idx
                    , onClick     $ \e _ -> stopPropagation e :
                        (dispatch ref $ UI.SearcherEvent $
                          AcceptWithHint (fromMaybe 0 selected + idx))
                    ] $ do
                    div_
                        ["key" $= "name"
                        ,"className" $= Style.prefix
                            "searcher__results__item__name"
                        ] $ result_ result
            when wait $ div_
                [ "key" $= "searcherResultsWaiting"
                , "className" $= Style.prefix "searcher__results__wait"
                ] $ elemString "Indexing hints, please wait..."

isSnippet :: Result Hint -> Bool
isSnippet = has $ Result.hint . Hint._Node . NodeHint.kind . NodeHint._Snippet

result_ :: Result Hint -> ReactElementM ViewEventHandler ()
result_ result = prefixElem >> expression where
    prefix     = convert $ result ^. SearcherData.prefix
    prefixElem = unless (isSnippet result) .
        span_ [ "className" $= Style.prefix "searcher__pre"
              , "key"       $= "searcherPre"
              ] . elemString $ if prefix == "" then prefix else prefix <> " . "
    expression = if has (Result.match . Match.source . Match._Expression) result
        then highlighted_ result
        else span_ [ "key" $= "l" ] . elemString . convert
                                    $ result ^. SearcherData.text


highlighted_ :: Result Hint -> ReactElementM ViewEventHandler ()
highlighted_ result = go 0 highlights where
    highlights = result ^. Result.match . Match.range
    name       = convert $ result ^. SearcherData.text
    go :: Int -> [Range] -> ReactElementM ViewEventHandler ()
    go omit [] = span_ [ "key" $= "l" ] . elemString
                                        $ drop omit name
    go omit (h:rest) = do
        let start       = h ^. Match.start
            len         = h ^. Match.length
            (r1, r2)    = splitAt start name
            normal      = drop omit r1
            highlighted = take len r2
        span_ [ "key" $= jsShow start ] $ do
            span_ [ "key" $= "n" ]
                $ elemString normal
            span_ [ "key" $= "h"
                  , "className" $= Style.prefix "searcher__hl" ]
                $ elemString highlighted
            go (start + len) rest
