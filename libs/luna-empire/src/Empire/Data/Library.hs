module Empire.Data.Library where

import qualified Empire.API.Data.Graph          as API (Graph)
import qualified Empire.API.Data.Library        as API
import qualified Empire.API.Persistence.Library as Persistence
import           Empire.Data.Graph              (Graph, defaultGraph)
import           Empire.Prelude


data Library = Library { _name    :: Maybe String
                       , _path    :: FilePath --TODO use smarter type
                       , _body    :: Graph
                       } deriving (Show)

make :: Maybe String -> FilePath -> IO Library
make name path = Library name path <$> defaultGraph

makeLenses ''Library

toAPI :: Library -> API.Library
toAPI (Library n p _) = API.Library n p

toPersistent :: Library -> API.Graph -> Persistence.Library
toPersistent (Library n p _) = Persistence.Library n p
