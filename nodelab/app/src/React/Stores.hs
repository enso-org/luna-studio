module React.Stores where

import           React.Flux
import           Utils.PreludePlus

import qualified React.Store.NodeEditor as NodeEditor



data Stores = Stores { _nodeEditor :: ReactStore NodeEditor.Store }

makeLenses ''Stores


create :: IO Stores
create = Stores
    <$> NodeEditor.create
