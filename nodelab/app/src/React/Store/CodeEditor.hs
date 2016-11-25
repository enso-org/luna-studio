{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.CodeEditor where

import           Control.DeepSeq   (NFData)
import           React.Flux
import           Utils.PreludePlus



data Store = Store { _code   :: Text
                   }

makeLenses ''Store

data Action = Action
            deriving (Show, Generic, NFData, Typeable)

instance StoreData Store where
    type StoreAction Store = Action
    transform _ = return

type Ref = ReactStore Store

create :: MonadIO m => m Ref
create = liftIO $ mkStore $ Store def
