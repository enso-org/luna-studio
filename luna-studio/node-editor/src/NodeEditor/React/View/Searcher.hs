{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Searcher where

import           Common.Prelude
import qualified Data.Text                       as Text
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
        mayCustomInput = if s ^. Searcher.replaceInput then ["value" $= convert (s ^. Searcher.inputText)] else []
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
                [ "key"         $= "searchInput"
                , "className"   $= Style.prefix "searcher__input"
                , "id"          $= searcherId
                , "placeholder" $= "Command search…"
                , onKeyDown     $ handleKeyDown ref
                , onKeyUp       $ \_ k -> dispatch ref $ UI.SearcherEvent $ KeyUp k
                , onChange      $ \e -> let val = target e "value"
                                            ss  = target e "selectionStart"
                                            se  = target e "selectionEnd"
                                        in dispatch ref $ UI.SearcherEvent $ InputChanged val ss se
                ] ++ mayCustomInput )
            div_
                [ "key"       $= "searcherResults"
                , "className" $= Style.prefix "searcher__results"
                ] $ do
                -- TODO [LJK, PM]: Refactor this piece of code:
                let selected = s ^. Searcher.selected
                case s ^. Searcher.mode of
                    Searcher.Command    results -> results_ ref selected results
                    Searcher.Node   _ _ results -> results_ ref selected results
                    Searcher.NodeName _ results -> results_ ref selected results
                    Searcher.PortName _ results -> results_ ref selected results
        -- div_
        --     [ "key"       $= "searcherPreview"
        --     , "className" $= Style.prefix "searcher__preview"
        --     ] $ withJust nodePreview $ nodeBody_ ref . (Node.position .~ nodePos)
                                              -- . (Node.isExpandedControls .~ True)

searcher_ :: Ref App -> Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref model = React.viewWithSKey searcher name (ref, model) mempty

results_ :: Ref App -> Int -> [Result.QueryResult r] -> ReactElementM ViewEventHandler ()
results_ ref selected results = forKeyed_ results $ \(idx, result) ->
    let resultClasses i   = Style.prefixFromList ( "searcher__results__item" : (if i + 1 == selected then [ "searcher__results__item--selected" ] else []))
        displayedName res = if Text.null $ result ^. Result.prefix
            then result ^. Result.name
            else result ^. Result.prefix <> " . " <> result ^. Result.name
    in div_
        [ "key"        $= jsShow idx
        , "className"  $= resultClasses idx
        , onClick      $ \e _ -> stopPropagation e : (dispatch ref $ UI.SearcherEvent $ AcceptEntry (idx + 1))
        , onMouseEnter $ \_ _ -> (dispatch ref $ UI.SearcherEvent $ SelectEntry (idx + 1))
        ] $ do
        div_
            ["key" $= "name"
            ,"className" $= Style.prefix "searcher__results__item__name"
            ] $ highlighted_ (convert $ displayedName result) $ result ^. Result.highlights

highlighted_ :: String -> [Result.Highlight] -> ReactElementM ViewEventHandler ()
highlighted_ text = highlighted_' 0 where
    highlighted_' :: Int -> [Result.Highlight] -> ReactElementM ViewEventHandler ()
    highlighted_' omit [] = span_ $ elemString $ snd $ splitAt omit text
    highlighted_' omit (highlight:rest) = do
        let start = highlight ^. Result.start
            len   = highlight ^. Result.length
            (r1         , r2    ) = splitAt start text
            (_          , normal) = splitAt omit r1
            (highlighted, _     ) = splitAt len r2
        span_ $ elemString normal
        span_ ["className" $= Style.prefix "searcher__results__item__highlighs"] $ elemString highlighted
        highlighted_' (start + len) rest
