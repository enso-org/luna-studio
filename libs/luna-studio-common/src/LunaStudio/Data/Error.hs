module LunaStudio.Data.Error where

import           Control.DeepSeq  (NFData)
import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Prologue

data NodeError  = CompileError | RuntimeError deriving (Eq, Generic, Show)
data GraphError = BreadcrumbDoesNotExist | ParseError | OtherGraphError deriving (Eq, Generic, Show)
data LunaError  = Graph GraphError | OtherLunaError deriving (Eq, Generic, Show)

data Error a = Error { _errorType    :: a
                     , _errorContent :: Text
                     } deriving (Eq, Generic, Show)

makeLenses ''Error
makePrisms ''NodeError
makePrisms ''GraphError

instance Binary   NodeError
instance NFData   NodeError
instance FromJSON NodeError
instance ToJSON   NodeError
instance Binary   GraphError
instance NFData   GraphError
instance FromJSON GraphError
instance ToJSON   GraphError
instance Binary   LunaError
instance NFData   LunaError
instance FromJSON LunaError
instance ToJSON   LunaError
instance Binary   a => Binary   (Error a)
instance NFData   a => NFData   (Error a)
instance ToJSON   a => ToJSON   (Error a)
instance FromJSON a => FromJSON (Error a)

instance (Typeable a, Show a) => Exception (Error a)
