module LunaStudio.Data.Error where

import           Control.DeepSeq  (NFData)
import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Prologue

data NodeError  = CompileError CompileErrorDetails | RuntimeError deriving (Generic, Show)
data GraphError = BreadcrumbDoesNotExist | ParseError | OtherGraphError deriving (Generic, Show)
data LunaError  = Graph GraphError | OtherLunaError deriving (Generic, Show)

data CompileErrorDetails = CompileErrorDetails { _arisingFrom :: [SourceLocation]
                                               , _requiredBy  :: [SourceLocation]
                                               } deriving (Generic, Show)

data SourceLocation = SourceLocation { _mod   :: Text
                                     , _klass :: Maybe Text
                                     , _fun   :: Text
                                     } deriving (Show, Generic)

data Error a = Error { _errorType    :: a
                     , _errorContent :: Text
                     } deriving (Generic, Show)

instance Eq (Error a) where
    _ == _ = False

makeLenses ''CompileErrorDetails
makeLenses ''Error
makeLenses ''SourceLocation
makePrisms ''NodeError
makePrisms ''GraphError

instance Binary   NodeError
instance NFData   NodeError
instance FromJSON NodeError
instance ToJSON   NodeError
instance Binary   SourceLocation
instance NFData   SourceLocation
instance FromJSON SourceLocation
instance ToJSON   SourceLocation
instance Binary   CompileErrorDetails
instance NFData   CompileErrorDetails
instance FromJSON CompileErrorDetails
instance ToJSON   CompileErrorDetails
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
