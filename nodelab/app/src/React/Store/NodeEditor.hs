{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.NodeEditor where

import           Control.DeepSeq             (NFData)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.IntMap                 (IntMap)
import           React.Flux
import           Utils.PreludePlus

import           Empire.API.Data.Node        (NodeId)
import qualified React.Store.Function.Input  as Input
import qualified React.Store.Function.Output as Output
import qualified React.Store.Node            as Node


data Store = Store { _nodes   :: HashMap NodeId Node.Ref
                   , _inputs  :: IntMap Input.Ref
                   , _outputs :: Maybe Output.Ref
                   }

makeLenses ''Store

data Action = Action
            deriving (Show, Generic, NFData, Typeable)

instance StoreData Store where
    type StoreAction Store = Action
    transform _ = return

type Ref = ReactStore Store

create :: MonadIO m => m Ref
create = liftIO $ mkStore $ Store HashMap.empty def def
