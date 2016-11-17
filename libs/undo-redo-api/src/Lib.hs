module Undo.API.Undo where

import Prologue
import Data.Binary (Binary)

import Empire.API.Graph.Request

data UndoItem =  UndoItem { _event :: Request } deriving (Show, Eq, Generic)

newtype UndoList = UndoList { _items :: [UndoItem] } deriving (Show, Eq, Generic)

makeLenses ''UndoItem

instance Binary UndoItem
instance Binary UndoList

-- topicPrefix = "empire.graph"
