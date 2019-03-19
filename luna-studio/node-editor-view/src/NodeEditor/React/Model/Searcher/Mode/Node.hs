{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Mode.Node where

import Common.Prelude

import qualified LunaStudio.Data.Searcher.Hint.Class as Class

import LunaStudio.Data.NodeLoc              (NodeId, NodeLoc)
import LunaStudio.Data.Port                 (OutPortId)
import LunaStudio.Data.PortRef              (OutPortRef)
import LunaStudio.Data.Position             (Position)
import NodeEditor.React.Model.Visualization (RunningVisualization)


-----------------
-- === New === --
-----------------

-- === Definition === --

data New = New
    { _position         :: Position
    , _connectionSource :: Maybe OutPortRef
    } deriving (Eq, Generic, Show)

makeLenses ''New

instance NFData New


------------------------
-- === Expression === --
------------------------

-- === Definition === --

data Expression = Expression
    { _newNode   :: Maybe New
    , _parent    :: Maybe Class.Name
    , _arguments :: [Text]
    } deriving (Eq, Generic, Show)

makeLenses ''Expression

instance NFData Expression


----------------------
-- === PortName === --
----------------------

-- === Definition === --

data PortName = PortName
    { _portId :: OutPortId
    } deriving (Eq, Generic, Show)

makeLenses ''PortName

instance NFData PortName


------------------
-- === Mode === --
------------------

-- === Definition === --

data Mode
    = ExpressionMode Expression
    | NodeNameMode
    | PortNameMode   PortName
    deriving (Eq, Generic, Show)

makePrisms ''Mode

instance NFData Mode


------------------
-- === Node === --
------------------

-- === Definition === --

data Node = Node
    { _nodeLoc                    :: NodeLoc
    , _documentationVisualization :: Maybe RunningVisualization
    , _mode                       :: Mode
    } deriving (Eq, Generic, Show)

makeLenses ''Node

instance NFData Node

-- === API === --

connectedPortRef :: Traversal' Node OutPortRef
connectedPortRef = mode . _ExpressionMode . newNode . _Just
                 . connectionSource . _Just
