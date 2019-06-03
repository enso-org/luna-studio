{-# LANGUAGE OverloadedStrings #-}
module Empire.Data.Library where

import Empire.Prelude

import qualified LunaStudio.Data.Graph              as API (Graph)
import qualified LunaStudio.Data.Library            as API

import Empire.Data.Graph (ClsGraph)
import Path              (Path, Rel, File)


data Library = Library { _name    :: Maybe String
                       , _path    :: Path Rel File
                       , _body    :: ClsGraph
                       } deriving (Show)

makeLenses ''Library
