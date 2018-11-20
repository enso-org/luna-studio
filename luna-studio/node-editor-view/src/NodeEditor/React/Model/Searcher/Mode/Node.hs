module NodeEditor.React.Model.Searcher.Mode.Node
    ( module NodeEditor.React.Model.Searcher.Mode.Node
    , module X
    ) where

import Common.Prelude
import LunaStudio.Data.Searcher.Node as X (ClassHints (ClassHints), ClassName,
                                           LibrariesHintsMap,
                                           LibraryHints (LibraryHints),
                                           LibraryName, Match, Name,
                                           NodeSearcherData (NodeSearcherData),
                                           TypePreference (TypePreference),
                                           getWeights, importedLibraries,
                                           libraries, localFunctionsLibraryName,
                                           missingLibraries,
                                           mkLocalFunctionsLibrary,
                                           notConnectedEmptyInputSearch, search,
                                           wildcardMatch)
import Prologue                      (unsafeFromJust)

import qualified Data.UUID.Types as UUID

import LunaStudio.Data.NodeLoc              (NodeId, NodeLoc)
import LunaStudio.Data.Port                 (OutPortId)
import LunaStudio.Data.PortRef              (OutPortRef)
import LunaStudio.Data.Position             (Position)
import NodeEditor.React.Model.Visualization (RunningVisualization)


data NewNodeData = NewNodeData
    { _position         :: Position
    , _connectionSource :: Maybe OutPortRef
    } deriving (Eq, Generic, Show)

makeLenses ''NewNodeData

data ExpressionData = ExpressionData
    { _newNodeData    :: Maybe NewNodeData
    , _className      :: Maybe ClassName
    , _argumentsNames :: [Name]
    } deriving (Eq, Generic, Show)

makeLenses ''ExpressionData

data PortNameData = PortNameData
    { _portId :: OutPortId
    } deriving (Eq, Generic, Show)

makeLenses ''PortNameData

data NodeSearcherMode
    = ExpressionMode ExpressionData
    | NodeNameMode
    | PortNameMode   PortNameData
    deriving (Eq, Generic, Show)

makePrisms ''NodeSearcherMode

data NodesData a = NodesData
    { _nodeLoc                    :: NodeLoc
    , _nodes                      :: [Match a]
    , _modeData                   :: NodeSearcherMode
    , _documentationVisualization :: Maybe RunningVisualization
    } deriving (Eq, Generic, Show)

makeLenses ''NodesData

--TODO[LJK]: Replace mockedSearcherNodId with:
-- mockedSearcherNodeId = unsafePerformIO genNewID
-- {-# NOINLINE mockedSearcherNodeId #-}

mockedSearcherNodeId :: NodeId
mockedSearcherNodeId = unsafeFromJust $ UUID.fromString nodeIdString where
    nodeIdString = "094f9784-3f07-40a1-84df-f9cf08679a27"

connectedPortRef :: Getter (NodesData a) (Maybe OutPortRef)
connectedPortRef = to $ (^? modeData . _ExpressionMode
    . newNodeData . _Just
    . connectionSource . _Just)
