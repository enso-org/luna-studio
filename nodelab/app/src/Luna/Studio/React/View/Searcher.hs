{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Searcher where

import qualified Data.Aeson                       as Aeson
import           Data.Text                        (unpack)
import           React.Flux
import qualified React.Flux                       as React

import qualified Event.UI                         as UI
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Searcher
import           Luna.Studio.React.Model.Searcher (Searcher)
import qualified Luna.Studio.React.Model.Searcher as Searcher
import           Luna.Studio.React.Store          (Ref, dispatch, dt)
import qualified Text.ScopeSearcher.QueryResult   as Result


name :: JSString
name = "searcher"


searcher :: Ref Searcher -> ReactView ()
searcher ref = React.defineControllerView
    name ref $ \store () -> do
        let s = store ^. dt
            pos = s ^. Searcher.position
        if s ^. Searcher.visible then do
            div_ [ "key"       $= name
                 , "className" $= name
                 , "style"     @= Aeson.object [ "top"  Aeson..= (show (pos ^. y) <> "px" :: String)
                                               , "left" Aeson..= (show (pos ^. x) <> "px" :: String)
                                               ]
                 ] $ do
                    input_
                        [ "key"   $= "input"
                        , "id"    $= "focus-searcher"
                        , "value" $= fromString (unpack $ s ^. Searcher.input)
                        , onMouseDown $ \e _ -> [stopPropagation e]
                        , onKeyDown   $ \e k ->  stopPropagation e : dispatch ref (UI.SearcherEvent $ KeyDown k)
                        , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.SearcherEvent $ InputChanged $ fromString val
                        ]
                    div_ [ "key"       $= "results"
                         , "className" $= "searcher-results"] $
                        forM_ (zip (s ^. Searcher.results) [0..]) $ \(result, idx) ->
                            div_ [ "key" $= fromString (show idx)
                                 , "className" $= if idx == s ^. Searcher.selected then "result-selected" else "result"] $ do
                                div_ ["key" $= "prefix"
                                     ,"className" $= "result-prefix"] $
                                    elemString $ fromString $ unpack (result ^. Result.prefix) <> "."
                                div_ ["key" $= "name"
                                     ,"className" $= "result-name"] $
                                    elemString $ fromString $ unpack $ result ^. Result.name
          else div_ ["key" $= name ] mempty


searcher_ :: Ref Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref = React.viewWithSKey (searcher ref) name () mempty


foreign import javascript safe "document.getElementById('focus-searcher').focus()" focus :: IO ()
