module React.Store.Searcher (
    module React.Store.Searcher,
) where

import           Utils.PreludePlus
import           Utils.Vector



data Searcher = Searcher
      { _visible :: Bool
      , _position :: Vector2 Int
      , _input :: Text
      } deriving (Show, Generic)

makeLenses ''Searcher

instance Default Searcher where
    def = Searcher False def def
