{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Searcher where

import qualified Data.Aeson                       as Aeson
import           Data.Vector
import           React.Flux
import qualified React.Flux                       as React

import qualified Luna.Studio.Event.UI             as UI
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Searcher
import           Luna.Studio.React.Model.App      (App)
import           Luna.Studio.React.Model.Searcher (Searcher)
import qualified Luna.Studio.React.Model.Searcher as Searcher
import           Luna.Studio.React.Store          (Ref, dispatch)
import qualified Text.ScopeSearcher.QueryResult   as Result


name :: JSString
name = "searcher"

searcher :: ReactView (Ref App, Searcher)
searcher  = React.defineView name $ \(ref, s) -> do
    let pos = s ^. Searcher.position
    div_
        [ "key"       $= name
        , "className" $= ("luna-" <> name)
        , "style"     @= Aeson.object
            [ "top"  Aeson..= (show (pos ^. y) <> "px" :: String)
            , "left" Aeson..= (show (pos ^. x) <> "px" :: String)
            ]
        ] $ do
        input_
            [ "key"   $= "input"
            , "id"    $= "focus-searcher"
            , "value" $= convert (s ^. Searcher.input)
            , onMouseDown $ \e _ -> [stopPropagation e]
            , onKeyDown   $ \e k -> stopPropagation e : dispatch ref (UI.SearcherEvent $ KeyDown k)
            , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.SearcherEvent $ InputChanged val
            ]
        div_
            [ "key"       $= "results"
            , "className" $= "luna-searcher-results"
            ] $ do
            forM_ (zip (s ^. Searcher.results) [0..]) $ \(result, idx) ->
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

foreign import javascript safe "document.getElementById('focus-searcher').focus()" focus :: IO ()
