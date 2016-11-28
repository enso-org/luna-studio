{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.Function.Input (
    module React.Store.Function.Input,
    module X
) where

import           Control.DeepSeq            (NFData)
import           Object.Widget.FunctionPort as X
import           Utils.PreludePlus


type Input = FunctionPort

data Action = Action
            deriving (Show, Generic, NFData, Typeable)
