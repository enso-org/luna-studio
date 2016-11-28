{-# LANGUAGE DeriveAnyClass #-}

module React.Store.Node (
    module React.Store.Node,
    module X,
) where

import           Control.DeepSeq    (NFData)
import           Utils.PreludePlus

import           Object.Widget.Node as X



data Event = OnClick
            deriving (Show, Generic, NFData, Typeable)
