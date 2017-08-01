module LunaStudio.Data.Graph where

import           Data.Binary               (Binary)
import           LunaStudio.Data.MonadPath (MonadPath)
import           LunaStudio.Data.Node      (ExpressionNode, InputSidebar, OutputSidebar)
import           LunaStudio.Data.PortRef   (InPortRef, OutPortRef)
import           Prologue


data Graph = Graph { _nodes         :: [ExpressionNode]
                   , _connections   :: [(OutPortRef, InPortRef)]
                   , _inputSidebar  :: Maybe InputSidebar
                   , _outputSidebar :: Maybe OutputSidebar
                   , _monads        :: [MonadPath]
                   } deriving (Eq, Generic, Show)

makeLenses ''Graph

instance Binary Graph
instance NFData Graph

instance Default Graph where
    def = Graph def def def def def
