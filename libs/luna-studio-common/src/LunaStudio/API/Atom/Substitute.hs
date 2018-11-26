module LunaStudio.API.Atom.Substitute where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Diff          (Diff)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.TextDiff      (TextDiff)
import           Prologue


data Request = Request
    { _location :: GraphLocation
    , _diffs    :: [TextDiff]
    } deriving (Eq, Generic, Show)

data Update = Update
    { _filePath' :: FilePath
    , _diffs'    :: [TextDiff]
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Update
instance NFData Update
instance ToJSON Update
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.atom.file.substitute"
instance T.MessageTopic Update where
    topic = T.topic @Request <> T.update
