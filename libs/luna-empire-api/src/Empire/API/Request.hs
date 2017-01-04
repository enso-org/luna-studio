module Empire.API.Request where

import           Prologue
import           Data.UUID.Types (UUID)
import           Data.Binary (Binary)
import qualified Empire.API.Topic as T

data Request a = Request { _requestId :: UUID
                         , _guiID  :: Maybe UUID
                         , _request   :: a
                         } deriving (Show, Eq, Generic)

makeLenses ''Request

instance (Binary a) => Binary (Request a)
