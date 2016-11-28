{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.Function.Output (
    module React.Store.Function.Output,
    module X
) where

import           Control.DeepSeq            (NFData)
import           Object.Widget.FunctionPort as X
import           Utils.PreludePlus



type Output = FunctionPort

data Action = Action
            deriving (Show, Generic, NFData, Typeable)
