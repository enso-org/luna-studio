module LunaStudio.API.Request where

import           Data.Binary     (Binary)
import           Data.UUID.Types (UUID)
import           Prologue


data Request a = Request { _requestId :: UUID
                         , _guiID     :: Maybe UUID
                         , _request   :: a
                         } deriving (Eq, Generic, Show)

makeLenses ''Request
instance Binary a => Binary (Request a)
