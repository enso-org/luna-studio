{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node.SidebarNode
    ( module Luna.Studio.React.Model.Node.SidebarNode
    , module X
    , NodeId
    , NodeLoc
    ) where

import           Control.Arrow                  ((&&&))
import           Data.Convert                   (Convertible (convert))
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.Map.Lazy                  as Map
import           Empire.API.Data.Node           (NodeId, inputEdgePorts)
import qualified Empire.API.Data.Node           as Empire
import           Empire.API.Data.NodeLoc        (NodeLoc (NodeLoc), NodePath)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.IsNode as X (IsNode (..))
import           Luna.Studio.React.Model.Port   (PortsMap, toPortsMap)
import qualified Luna.Studio.React.Model.Port   as Port


data SidebarType = InputSidebar | OutputSidebar  deriving (Generic, Eq, NFData, Show)
data SidebarMode = AddRemove
              | MoveConnect
              deriving (Generic, Eq, NFData, Show)

instance Default SidebarMode where
    def = MoveConnect

data SidebarNode = SidebarNode { _nodeLoc'    :: NodeLoc
                               , _sidebarType :: SidebarType
                               , _ports'      :: PortsMap
                               , _mode        :: SidebarMode
                               } deriving (Eq, Generic, NFData, Show)

makeLenses ''SidebarNode

type SidebarNodesMap = HashMap NodeId SidebarNode

instance Convertible (NodePath, Empire.Node, SidebarType) SidebarNode where
    convert (path, n, type') = SidebarNode
        {- nodeLoc     -} (NodeLoc path (n ^. Empire.nodeId))
        {- sidebarType -} type'
        {- ports       -} ( case type' of
            OutputSidebar -> convert <$> n ^. Empire.ports
            InputSidebar  ->
                toPortsMap . convert . maybe [] id $ n ^? Empire.nodeType . inputEdgePorts
            )
        {- mode        -} def

instance IsNode SidebarNode where
    nodeLoc              = nodeLoc'
    ports                = ports'
    getPorts             = Map.elems . view ports
    hasPort pid          = Map.member pid . view ports
    countInPorts         = Port.countInPorts . Map.keys . (view ports)
    countOutPorts        = Port.countOutPorts . Map.keys . (view ports)
    countArgPorts        = Port.countArgPorts . Map.keys . (view ports)
    countProjectionPorts = Port.countProjectionPorts . Map.keys . (view ports)

toSidebarNodesMap :: [SidebarNode] -> SidebarNodesMap
toSidebarNodesMap = HashMap.fromList . map (view nodeId &&& id)


isInputSidebar :: SidebarNode -> Bool
isInputSidebar n = n ^. sidebarType == InputSidebar

isOutputSidebar :: SidebarNode -> Bool
isOutputSidebar n = n ^. sidebarType == OutputSidebar

isInMode :: SidebarMode -> SidebarNode -> Bool
isInMode mode' n = n ^. mode == mode'

isInAddRemoveMode :: SidebarNode -> Bool
isInAddRemoveMode = isInMode AddRemove

isInMoveConnectMode :: SidebarNode -> Bool
isInMoveConnectMode = isInMode MoveConnect
