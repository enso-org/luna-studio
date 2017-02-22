{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Searcher where

import qualified Data.Aeson                       as Aeson
import           Data.Matrix                      as Matrix
import           Data.Matrix                      (Matrix)
import           Data.Position                    (Position (Position), Vector2 (Vector2))
import           Data.Vector
import           React.Flux
import qualified React.Flux                       as React

import           JS.Searcher                      (searcherId)
import qualified Luna.Studio.Event.Keys           as Keys
import qualified Luna.Studio.Event.UI             as UI
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Searcher
import           Luna.Studio.React.Model.App      (App)
import qualified Luna.Studio.React.Model.Node     as Node
import           Luna.Studio.React.Model.Searcher (Searcher)
import qualified Luna.Studio.React.Model.Searcher as Searcher
import           Luna.Studio.React.Store          (Ref, dispatch)
import           Luna.Studio.React.View.Node      (expressionPosition)
import           Luna.Studio.React.View.NodeBody  (nodeBody_)
import qualified Text.ScopeSearcher.QueryResult   as Result


name :: JSString
name = "searcher"

preventTabDefault :: React.Event -> KeyboardEvent -> [SomeStoreAction] -> [SomeStoreAction]
preventTabDefault e k r = if Keys.withoutMods k Keys.tab then preventDefault e : r else r

searcher :: ReactView (Ref App, Searcher, Matrix Double)
searcher =  React.defineView name $ \(ref, s, camera) -> do
    let pos       = expressionPosition camera (s ^. Searcher.position)
        nodePos   = Position (Vector2 0 0)
        mode      = s ^. Searcher.mode
        nodePrev  = Node.fromNode <$> s ^. Searcher.selectedNode
        className = "luna-" <> name <> " luna-" <> case mode of
                                                        Searcher.Node    { } -> "node-"    <> name
                                                        Searcher.Command { } -> "command-" <> name
    div_
        [ "key"       $= name
        , "className" $= className
        , "style"     @= Aeson.object
            [ "transform" Aeson..= ("translate(" <> show (pos ^. x) <> "px, " <> show (pos ^. y) <> "px)" :: String) ]
        ] $ do
        withJust nodePrev $ nodeBody_ ref . (Node.position .~ nodePos)
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

searcher_ :: Ref App -> Searcher -> Matrix Double -> ReactElementM ViewEventHandler ()
searcher_ ref model camera = React.viewWithSKey searcher name (ref, model, camera) mempty
