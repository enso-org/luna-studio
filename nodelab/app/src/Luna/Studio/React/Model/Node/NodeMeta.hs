{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Luna.Studio.React.Model.Node.NodeMeta where

import           Data.Convert             (Convertible (convert))
import           Data.Position            (Position, fromTuple, toTuple)
import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.NodeMeta (NodeMeta (NodeMeta))
import           Luna.Studio.Prelude


instance Convertible (Position, Bool) NodeMeta where
    convert (pos, dispRes) = NodeMeta (toTuple pos) dispRes

instance Convertible NodeMeta (Position, Bool) where
    convert (NodeMeta pos dispRes) = (fromTuple pos, dispRes)


instance Convertible (NodeId, Position, Bool) (NodeId, NodeMeta) where
    convert (nid, pos, dispRes) = (nid, convert (pos, dispRes))

instance Convertible (NodeId, NodeMeta) (NodeId, Position, Bool) where
    convert (nid, NodeMeta pos dispRes) = (nid, fromTuple pos, dispRes)
