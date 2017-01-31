module Text.ScopeSearcher.QueryResult where

import           Control.Lens
import           Data.Text                (Text)
import           GHC.Generics             (Generic)

import           Text.ScopeSearcher.Score (Score)


data Highlight = Highlight { _start :: Int
                           , _len   :: Int
                           } deriving (Show, Eq, Generic)

data QueryResult = QueryResult { _prefix     :: Text
                               , _name       :: Text
                               , _fullname   :: Text
                               , _highlights :: [Highlight]
                               , _tpe        :: Text
                               , _score      :: Score
                               } deriving (Show, Eq, Generic)

makeLenses ''Highlight
makeLenses ''QueryResult
