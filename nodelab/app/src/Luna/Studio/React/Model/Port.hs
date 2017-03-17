{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Model.Port
    ( module Luna.Studio.React.Model.Port
    , module X
    )
    where

import           Control.Arrow               ((&&&))
import           Data.Aeson                  (ToJSON)
import           Data.Convert                (Convertible (convert))
import           Data.Map.Lazy               (Map)
import qualified Data.Map.Lazy               as Map
import           Data.Position               (Position)
import           Empire.API.Data.Port        as X (InPort (..), OutPort (..), PortId (..), PortState (..), getPortNumber, isAll, isArg,
                                                   isInPort, isOutPort, isProjection, isSelf, _InPortId, _OutPortId)
import qualified Empire.API.Data.Port        as Empire
import           Empire.API.Data.PortDefault as X (PortDefault (..))
import           Empire.API.Data.PortRef     as X (AnyPortRef (InPortRef', OutPortRef'), InPortRef (InPortRef), OutPortRef (OutPortRef))
import           Empire.API.Data.TypeRep     (TypeRep (..))
import           Luna.Studio.Data.Color      (Color)
import qualified Luna.Studio.Data.Color      as Color
import           Luna.Studio.Prelude         hiding (set)


data Port = Port { _portId    :: PortId
                 , _name      :: String
                 , _valueType :: TypeRep
                 , _state     :: PortState
                 , _color     :: Color
                 , _highlight :: Bool
                 , _visible   :: Bool
                 , _isEdited  :: Bool
                 } deriving (Eq, Show, Typeable, Generic, NFData)

makeLenses ''Port
instance ToJSON Port

data DraggedPort = DraggedPort { _draggedPort       :: Port
                               , _positionInSidebar :: Position
                               } deriving (Eq, Show, Typeable, Generic, NFData)

makeLenses ''DraggedPort
instance ToJSON DraggedPort

toPortsMap :: [Port] -> Map PortId Port
toPortsMap = Map.fromList . map (view portId &&& id)

countInPorts :: [PortId] -> Int
countInPorts = foldl (\acc pid -> acc + if isInPort pid then 1 else 0) 0

countOutPorts :: [PortId] -> Int
countOutPorts = foldl (\acc pid -> acc + if isOutPort pid then 1 else 0) 0

countArgPorts :: [PortId] -> Int
countArgPorts = foldl (\acc pid -> acc + if isArg pid then 1 else 0) 0

countProjectionPorts :: [PortId] -> Int
countProjectionPorts = foldl (\acc pid -> acc + if isProjection pid then 1 else 0) 0


instance Convertible Empire.Port Port where
    convert p = Port
        {- portId    -} (p ^. Empire.portId)
        {- name      -} (p ^. Empire.name)
        {- nodeType  -} (p ^. Empire.valueType)
        {- state     -} (p ^. Empire.state)
        {- color     -} (Color.fromType $ p ^. Empire.valueType)
        {- highlight -} False
        {- visible   -} True
        {- isEdited  -} False

instance Convertible Port Empire.Port where
    convert p = Empire.Port
        {- portId    -} (p ^. portId)
        {- name      -} (p ^. name)
        {- nodeType  -} (p ^. valueType)
        {- state     -} (p ^. state)
