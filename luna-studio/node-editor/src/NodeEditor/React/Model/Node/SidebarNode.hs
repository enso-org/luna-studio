{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module NodeEditor.React.Model.Node.SidebarNode
    ( module NodeEditor.React.Model.Node.SidebarNode
    , module X
    , NodeId
    , NodeLoc
    ) where

import           Common.Prelude
import           Data.Convert                  (Convertible (convert))
import           Data.HashMap.Strict           (HashMap)
import           LunaStudio.Data.Node          (NodeId)
import qualified LunaStudio.Data.Node          as Empire
import           LunaStudio.Data.NodeLoc       (NodeLoc (NodeLoc), NodePath)
import           NodeEditor.React.Model.IsNode as X
import           NodeEditor.React.Model.Port   (InPort, InPortTree, OutPort, OutPortIndex (Projection), OutPortTree)
import qualified NodeEditor.React.Model.Port   as Port


data SidebarMode = AddRemove
              | MoveConnect
              deriving (Generic, Eq, NFData, Show)

instance Default SidebarMode where
    def = MoveConnect

type Height = Double

data InputNode = InputNode
        { _inputNodeLoc      :: NodeLoc
        , _inputSidebarPorts :: [OutPortTree OutPort]
        , _inputMode         :: SidebarMode
        -- TODO[LJK, PM]: We should store OutPortId here but then lenses are invalid
        -- , _inputFrozenState  :: Maybe (Height, Maybe AnyPortId)
        } deriving (Eq, Generic, NFData, Show)

data OutputNode = OutputNode
        { _outputNodeLoc      :: NodeLoc
        , _outputSidebarPorts :: InPortTree InPort
        , _outputMode         :: SidebarMode
        -- , _outputFrozenState  :: Maybe (Height, Maybe AnyPortId)
        } deriving (Eq, Generic, NFData, Show)

makeLenses ''InputNode
makeLenses ''OutputNode

type InputNodesMap  = HashMap NodeId InputNode
type OutputNodesMap = HashMap NodeId OutputNode

instance Convertible (NodePath, Empire.InputSidebar) InputNode where
    convert (path, n) = InputNode
        {- inputNodeLoc      -} (NodeLoc path (n ^. Empire.inputNodeId))
        {- inputSidebarPorts -} (convert `fmap2` (n ^. Empire.inputEdgePorts))
        {- inputMode         -} def
        -- {- inputFrozenState  -} def

instance Convertible (NodePath, Empire.OutputSidebar) OutputNode where
    convert (path, n) = OutputNode
        {- outputNodeLoc      -} (NodeLoc path (n ^. Empire.outputNodeId))
        {- outputSideBarPorts -} (convert <$> n ^. Empire.outputEdgePorts)
        {- outputMode         -} def
        -- {- outputFrozenState  -} def

instance HasNodeLoc InputNode where
    nodeLoc = inputNodeLoc

instance HasNodeLoc OutputNode where
    nodeLoc = outputNodeLoc

instance HasPorts OutputNode where
    inPortsList  = Port.inPortTreeLeafs . view outputSidebarPorts
    outPortsList = const def
    inPortAt pid = outputSidebarPorts . ix pid
    outPortAt    = const ignored

instance HasPorts InputNode where
    inPortsList   = const def
    outPortsList  = concatMap Port.outPortTreeLeafs . view inputSidebarPorts
    inPortAt      = const ignored
    outPortAt ((Projection i):t) = inputSidebarPorts . ix i . ix t
    outPortAt _                  = ignored

class IsNode node => SidebarNode node where
    mode           :: Lens' node SidebarMode
    -- frozenState    :: Lens' node (Maybe (Height, Maybe AnyPortId))
    isInputSidebar :: node -> Bool

    isInMode :: SidebarMode -> node -> Bool
    isInMode mode' n = n ^. mode == mode'

    isInAddRemoveMode :: node -> Bool
    isInAddRemoveMode = isInMode AddRemove

    isInMoveConnectMode :: node -> Bool
    isInMoveConnectMode = isInMode MoveConnect

instance SidebarNode InputNode where
    mode           = inputMode
    -- frozenState    = inputFrozenState
    isInputSidebar = const True

instance SidebarNode OutputNode where
    mode           = outputMode
    -- frozenState    = outputFrozenState
    isInputSidebar = const False
