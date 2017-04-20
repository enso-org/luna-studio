{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module NodeEditor.React.Model.Node.NodeMeta where

import           Data.Convert             (Convertible (convert))
import           Data.Position            (Position, fromTuple, toTuple)
import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.NodeLoc  (NodeLoc (NodeLoc), NodePath, HasNodeLoc(..))
import           Empire.API.Data.NodeMeta (NodeMeta (NodeMeta))
import           Common.Prelude


instance Convertible (Position, Bool) NodeMeta where
    convert (pos, dispRes) = NodeMeta (toTuple pos) dispRes

instance Convertible NodeMeta (Position, Bool) where
    convert (NodeMeta pos dispRes) = (fromTuple pos, dispRes)

instance HasNodeLoc (NodeLoc, NodeMeta) where
    nodeLoc = _1

instance Convertible (a, Position, Bool) (a, NodeMeta) where
    convert (a, pos, dispRes) = (a, convert (pos, dispRes))

instance Convertible (a, NodeMeta) (a, Position, Bool) where
    convert (a, NodeMeta pos dispRes) = (a, fromTuple pos, dispRes)

instance Convertible (NodePath, (NodeId, NodeMeta)) (NodeLoc, Position, Bool) where
    convert (path, (nid, meta)) = convert (NodeLoc path nid, meta)
