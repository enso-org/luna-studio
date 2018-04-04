module LunaStudio.Data.Graph where

import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.Binary                (Binary)
import           Data.Map                   (Map)
import           LunaStudio.Data.Connection (Connection, ConnectionId)
import           LunaStudio.Data.MonadPath  (MonadPath)
import           LunaStudio.Data.Node       (ExpressionNode, InputSidebar, OutputSidebar)
import           LunaStudio.Data.NodeLoc    (NodeLoc)
import           Prologue


data Graph = Graph
    { _nodes         :: Map NodeLoc ExpressionNode
    , _connections   :: Map ConnectionId Connection
    , _inputSidebar  :: Maybe InputSidebar
    , _outputSidebar :: Maybe OutputSidebar
    , _monads        :: [MonadPath]
    } deriving (Eq, Generic, Show)

makeLenses ''Graph

instance Binary   Graph
instance NFData   Graph
instance FromJSON Graph
instance ToJSON   Graph

instance Default Graph where
    def = Graph def def def def def
