{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization.DataFrame where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.DataFrame (DataFrame)
import qualified Luna.Studio.React.Model.DataFrame as DataFrame
import           React.Flux


dataFrame_ :: Int -> DataFrame -> ReactElementM ViewEventHandler ()
dataFrame_ visIx df =
    div_
        [ "key" $= convert (show visIx)
        , "className" $= "vis vis--table"
        ] $ do
        div_
            [ "className" $= "blur" ] mempty
        table_ $ do
            thead_ $ do
                tr_ $ forM_ (df ^. DataFrame.headers) $
                    th_ .
                        elemString . convert
        div_
            [ "className" $= "scroll"
            ] $ do
            table_ $
                tbody_ $
                    forM_ (df ^. DataFrame.rows) $ \row ->
                        tr_ $ forM_ row $
                            td_ .
                                elemString . convert
