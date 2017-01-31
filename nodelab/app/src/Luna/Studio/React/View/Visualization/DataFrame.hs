{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization.DataFrame where

import qualified Data.Text                         as Text
import           React.Flux
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.DataFrame (DataFrame)
import qualified Luna.Studio.React.Model.DataFrame as DataFrame


dataFrame_ :: Int -> DataFrame -> ReactElementM ViewEventHandler ()
dataFrame_ visIx df =
    div_
        [ "key" $= fromString (show visIx)
        , "className" $= "vis vis--table"
        ] $ do
        div_
            [ "className" $= "blur" ] mempty
        table_ $ do
            thead_ $ do
                tr_ $ forM_ (df ^. DataFrame.headers) $
                    th_ .
                        elemString . fromString . Text.unpack
        div_
            [ "className" $= "scroll"
            ] $ do
            table_ $
                tbody_ $
                    forM_ (df ^. DataFrame.rows) $ \row ->
                        tr_ $ forM_ row $
                            td_ .
                                elemString . fromString . Text.unpack
