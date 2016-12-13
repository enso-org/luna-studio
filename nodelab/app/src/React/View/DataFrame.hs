{-# LANGUAGE OverloadedStrings #-}
module React.View.DataFrame where

import qualified Data.Text.Lazy          as Text
import           React.Flux

import           Object.Widget.DataFrame (DataFrame)
import qualified Object.Widget.DataFrame as DataFrame
import           Utils.PreludePlus



dataFrame_ :: Int -> DataFrame -> ReactElementM ViewEventHandler ()
dataFrame_ visIx df =
    foreignObject_
        [ "key" $= fromString (show visIx)
        , "className" $= "data-frame" ] $
        div_
            [ "xmlns" $= "http://www.w3.org/1999/xhtml"] $
            table_ $
                tbody_ $ do
                    tr_ $ forM_ (df ^. DataFrame.headers) $
                        th_ .
                            elemString . fromString . Text.unpack
                    forM_ (df ^. DataFrame.rows) $ \row ->
                        tr_ $ forM_ row $
                            td_ .
                                elemString . fromString . Text.unpack

foreignObject_ :: Term eventHandler arg result => arg -> result
foreignObject_ = term "foreignObject"
