{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.Function.Output where

import           Control.DeepSeq            (NFData)
import           Object.Widget.FunctionPort (FunctionPort)
import           React.Flux
import           Utils.PreludePlus



data Store = Store { _input :: FunctionPort }
           deriving (Show, Generic)

data Action = Action
            deriving (Show, Generic, NFData, Typeable)

instance StoreData Store where
    type StoreAction Store = Action
    transform _  = return

type Ref = ReactStore Store

create :: MonadIO m => FunctionPort -> m Ref
create = liftIO . mkStore . Store
