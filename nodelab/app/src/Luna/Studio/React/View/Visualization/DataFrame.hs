{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization.DataFrame where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.DataFrame (DataFrame)
import qualified Luna.Studio.React.Model.DataFrame as DataFrame
import           React.Flux


dataFrame_ :: Int -> DataFrame -> ReactElementM ViewEventHandler ()
dataFrame_ visIx df =
    div_
        [ "key" $= jsShow visIx
        , "className" $= "luna-vis luna-vis--table"
        ] $ do
        div_
            [ "key"       $= "blur"
            , "className" $= "luna-blur" ] mempty
        table_ [ "key" $= "table" ] $
            thead_ $
                tr_ $ forKeyed_ (df ^. DataFrame.headers) $ \(i, header) ->
                    th_ [ "key" $= jsShow i ].
                        elemString $ convert header
        div_
            [ "key"       $= "scroll"
            , "className" $= "luna-scroll"
            , onWheel     $ \e _ _ -> [stopPropagation e]
            ] $
            table_ $
                tbody_ $
                    forKeyed_ (df ^. DataFrame.rows) $ \(ri, row) ->
                        tr_ [ "key" $= jsShow ri ]$ forKeyed_ row $ \(di, d) ->
                            td_ [ "key" $= jsShow di ] $
                                elemString $ convert d
