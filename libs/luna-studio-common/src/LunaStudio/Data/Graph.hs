module LunaStudio.Data.Graph where

import Prologue

import qualified LunaStudio.Data.Searcher.Hint.Library as Library

import Data.Aeson.Types           (FromJSON, ToJSON)
import Data.Binary                (Binary)
import Data.Set                   (Set)
import LunaStudio.Data.Connection (Connection)
import LunaStudio.Data.MonadPath  (MonadPath)
import LunaStudio.Data.Node       (ExpressionNode, InputSidebar, OutputSidebar)


data Graph = Graph
    { _nodes         :: [ExpressionNode]
    , _connections   :: [Connection]
    , _inputSidebar  :: Maybe InputSidebar
    , _outputSidebar :: Maybe OutputSidebar
    , _monads        :: [MonadPath]
    , _imports       :: Set Library.Name
    } deriving (Eq, Generic, Show)

makeLenses ''Graph

instance Binary   Graph
instance NFData   Graph
instance FromJSON Graph
instance ToJSON   Graph

instance Default Graph where
    def = Graph def def def def def def
