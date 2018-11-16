{-# LANGUAGE OverloadedStrings #-}
module Empire.Data.Library where

import qualified LunaStudio.Data.Graph          as API (Graph)
import qualified LunaStudio.Data.Library        as API
import qualified LunaStudio.API.Persistence.Library as Persistence
import           Empire.Data.Graph              (ClsGraph)
import           Empire.Prelude


data Library = Library { _name    :: Maybe String
                       , _path    :: FilePath --TODO use smarter type
                       , _body    :: ClsGraph
                       } deriving (Show)

makeLenses ''Library

toAPI :: Library -> API.Library
toAPI (Library n p _) = API.Library n p

toPersistent :: Library -> API.Graph -> Persistence.Library
toPersistent (Library n p _) = Persistence.Library n p
