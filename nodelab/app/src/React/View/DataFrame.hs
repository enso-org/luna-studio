{-# LANGUAGE OverloadedStrings #-}
module React.View.DataFrame where

import           React.Flux

import           Object.Widget.DataFrame (DataFrame)
import qualified Object.Widget.DataFrame as DataFrame
import           Utils.PreludePlus



dataFrame_ :: Int -> DataFrame -> ReactElementM ViewEventHandler ()
dataFrame_ visIx df =
    text_
        ["className" $= "name"] $
            elemString $ fromString $ "adadadajdhakjdhkadk" <> show df
