{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.App where

import           Control.DeepSeq        (NFData)
import           React.Flux
import           Utils.PreludePlus

import qualified React.Store.NodeEditor as NodeEditor



data Store = Store { _nodeEditor :: NodeEditor.Ref
                   }

makeLenses ''Store

data Action = Action
            deriving (Show, Generic, NFData, Typeable)

instance StoreData Store where
    type StoreAction Store = Action
    transform _ = return

type Ref = ReactStore Store

create :: MonadIO m => m Ref
create = liftIO . mkStore . Store =<< NodeEditor.create
