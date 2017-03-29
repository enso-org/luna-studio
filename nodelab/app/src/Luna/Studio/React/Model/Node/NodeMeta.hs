{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node.NodeMeta where

import           Data.Convert             (Convertible (convert))
import           Data.Position            (Position, fromTuple, toTuple)
import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.NodeLoc  (NodeLoc (NodeLoc), NodePath)
import           Empire.API.Data.NodeMeta (NodeMeta (NodeMeta))
import           Luna.Studio.Prelude


instance Convertible (Position, Bool) NodeMeta where
    convert (pos, dispRes) = NodeMeta (toTuple pos) dispRes

instance Convertible NodeMeta (Position, Bool) where
    convert (NodeMeta pos dispRes) = (fromTuple pos, dispRes)


instance Convertible (NodeLoc, Position, Bool) (NodeLoc, NodeMeta) where
    convert (nl, pos, dispRes) = (nl, convert (pos, dispRes))

instance Convertible (NodeLoc, NodeMeta) (NodeLoc, Position, Bool) where
    convert (nl, NodeMeta pos dispRes) = (nl, fromTuple pos, dispRes)

instance Convertible (NodePath, (NodeId, NodeMeta)) (NodeLoc, Position, Bool) where
    convert (path, (nid, meta)) = convert (NodeLoc path nid, meta)
