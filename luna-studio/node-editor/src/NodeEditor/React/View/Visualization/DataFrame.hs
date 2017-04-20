{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Visualization.DataFrame where

import           Common.Prelude
import           NodeEditor.React.Model.DataFrame (DataFrame)
import qualified NodeEditor.React.Model.DataFrame as DataFrame
import qualified NodeEditor.React.View.Style      as Style
import           React.Flux


dataFrame_ :: Int -> DataFrame -> ReactElementM ViewEventHandler ()
dataFrame_ visIx df =
    div_
        [ "key" $= jsShow visIx
        , "className" $= Style.prefixFromList ["vis", "vis--table"]
        ] $ do
        div_
            [ "key"       $= "blur"
            , "className" $= Style.prefix "blur"
            ] mempty
        table_ [ "key" $= "table" ] $
            thead_ $
                tr_ $ forKeyed_ (df ^. DataFrame.headers) $ \(i, header) ->
                    th_ [ "key" $= jsShow i ].
                        elemString $ convert header
        div_
            [ "key"       $= "scroll"
            , "className" $= Style.prefix "scroll"
            , onWheel     $ \e _ _ -> [stopPropagation e]
            ] $
            table_ $
                tbody_ $
                    forKeyed_ (df ^. DataFrame.rows) $ \(ri, row) ->
                        tr_ [ "key" $= jsShow ri ]$ forKeyed_ row $ \(di, d) ->
                            td_ [ "key" $= jsShow di ] $
                                elemString $ convert d
