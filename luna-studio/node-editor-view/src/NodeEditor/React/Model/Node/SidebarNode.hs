{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE StrictData        #-}
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
import qualified LunaStudio.Data.LabeledTree   as LabeledTree
import qualified LunaStudio.Data.Node          as Empire
import           LunaStudio.Data.NodeId        (NodeId)
import           LunaStudio.Data.NodeLoc       (NodeLoc (NodeLoc), NodePath)
import           NodeEditor.React.Model.IsNode as X
import           NodeEditor.React.Model.Port   (AnyPortId (InPortId', OutPortId'), InPort, InPortTree, OutPort, OutPortIndex (Projection),
                                                OutPortTree)
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
        , _inputIsDef        :: Bool
        , _inputMode         :: SidebarMode
        } deriving (Eq, Generic, NFData, Show)

data OutputNode = OutputNode
        { _outputNodeLoc      :: NodeLoc
        , _outputSidebarPorts :: InPortTree InPort
        , _outputMode         :: SidebarMode
        } deriving (Eq, Generic, NFData, Show)

makeLenses ''InputNode
makeLenses ''OutputNode

type InputNodesMap  = HashMap NodeId InputNode
type OutputNodesMap = HashMap NodeId OutputNode

instance Convertible (NodePath, Empire.InputSidebar) InputNode where
    convert (path, n) = InputNode
        {- inputNodeLoc      -} (NodeLoc path (n ^. Empire.inputNodeId))
        {- inputSidebarPorts -} (convert `fmap2` (n ^. Empire.inputEdgePorts))
        {- inputIsDef        -} (n ^. Empire.isDef)
        {- inputMode         -} def

instance Convertible (NodePath, Empire.OutputSidebar) OutputNode where
    convert (path, n) = OutputNode
        {- outputNodeLoc      -} (NodeLoc path (n ^. Empire.outputNodeId))
        {- outputSideBarPorts -} (convert <$> n ^. Empire.outputEdgePorts)
        {- outputMode         -} def

instance HasNodeLoc InputNode where
    nodeLoc = inputNodeLoc

instance HasNodeLoc OutputNode where
    nodeLoc = outputNodeLoc

instance HasPorts OutputNode where
    inPortsList  = return . view LabeledTree.value . view outputSidebarPorts
    outPortsList = const def
    inPortAt pid = outputSidebarPorts . ix pid
    outPortAt    = const ignored
    portModeAt = \case
        OutPortId' outpid -> outPortAt outpid . Port.mode
        InPortId'  inpid  -> inPortAt  inpid  . Port.mode


instance HasPorts InputNode where
    inPortsList   = const def
    outPortsList  = concatMap Port.visibleOutPorts . view inputSidebarPorts
    inPortAt      = const ignored
    outPortAt (Projection i : t) = inputSidebarPorts . ix i . ix t
    outPortAt _                  = ignored
    portModeAt = \case
        OutPortId' outpid -> outPortAt outpid . Port.mode
        InPortId'  inpid  -> inPortAt  inpid  . Port.mode

class (IsNode node, Typeable node, Eq node) => SidebarNode node where
    mode                 :: Lens' node SidebarMode
    minimalNumberOfPorts :: node -> Int
    isInputSidebar       :: node -> Bool

    isInMode :: SidebarMode -> node -> Bool
    isInMode mode' n = n ^. mode == mode'

    isInAddRemoveMode :: node -> Bool
    isInAddRemoveMode = isInMode AddRemove

    isInMoveConnectMode :: node -> Bool
    isInMoveConnectMode = isInMode MoveConnect

instance SidebarNode InputNode where
    mode                   = inputMode
    minimalNumberOfPorts n = if n ^. inputIsDef then 0 else 1
    isInputSidebar         = const True

instance SidebarNode OutputNode where
    mode                 = outputMode
    minimalNumberOfPorts = const 1
    isInputSidebar       = const False
