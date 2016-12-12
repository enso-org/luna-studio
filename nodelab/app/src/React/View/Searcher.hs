{-# LANGUAGE OverloadedStrings #-}
module React.View.Searcher where

import qualified Data.Aeson                     as Aeson
import           Data.Text.Lazy                 (unpack)
import           React.Flux
import qualified React.Flux                     as React

import qualified Event.UI                       as UI
import           React.Event.Searcher
import           React.Store                    (Ref, dispatch, dt)
import           React.Store.Searcher           (Searcher)
import qualified React.Store.Searcher           as Searcher
import qualified Text.ScopeSearcher.QueryResult as Result
import           Utils.PreludePlus
import           Utils.Vector


name :: JSString
name = "searcher"


searcher :: Ref Searcher -> ReactView ()
searcher ref = React.defineControllerView
    name ref $ \store () -> do
        let s = store ^. dt
            pos = s ^. Searcher.position
        when (s ^. Searcher.visible) $ do
            div_ [ "className" $= "searcher"
                 , "style"     @= Aeson.object [ "top"  Aeson..= (show (pos ^. y) <> "px" :: String)
                                               , "left" Aeson..= (show (pos ^. x) <> "px" :: String)
                                               ]
                 ] $ do
                    input_
                        [ "id" $= "focus-searcher"
                        , "value" $= fromString (unpack $ s ^. Searcher.input)
                        , onMouseDown $ \e _ -> [stopPropagation e]
                        , onKeyDown   $ \e k ->  stopPropagation e : dispatch ref (UI.SearcherEvent $ KeyDown k)
                        , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.SearcherEvent $ InputChanged $ fromString val
                        ]
                    div_ ["className" $= "searcher-results"] $
                        forM_ (zip (s ^. Searcher.results) [0..]) $ \(result, idx) ->
                            div_ ["className" $= if idx == s ^. Searcher.selected then "result-selected" else "result"] $ do
                                div_ ["className" $= "result-prefix"] $
                                    elemString $ fromString $ unpack (result ^. Result.prefix) <> "."
                                div_ ["className" $= "result-name"] $
                                    elemString $ fromString $ unpack $ result ^. Result.name


searcher_ :: Ref Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref = React.view (searcher ref) () mempty


foreign import javascript safe "document.getElementById('focus-searcher').focus()" focus :: IO ()
