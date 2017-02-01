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
        , "className" $= "vis vis--table"
        ] $ do
        div_
            [ "key"       $= "blur"
            , "className" $= "blur" ] mempty
        table_ [ "key" $= "table" ] $
            thead_ $
                tr_ $ forM_ (keyed $ df ^. DataFrame.headers) $ \(i, header) ->
                    th_ [ "key" $= jsShow i ].
                        elemString $ convert header
        div_
            [ "key"       $= "scroll"
            , "className" $= "scroll"
            , onWheel     $ \e _ _ -> [stopPropagation e]
            ] $
            table_ $
                tbody_ $
                    forM_ (keyed $ df ^. DataFrame.rows) $ \(ri, row) ->
                        tr_ [ "key" $= jsShow ri ]$ forM_ (keyed row) $ \(di, d) ->
                            td_ [ "key" $= jsShow di ] $
                                elemString $ convert d

keyed :: [a] -> [(Int, a)]
keyed = zip [0..]
