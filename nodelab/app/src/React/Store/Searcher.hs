module React.Store.Searcher (
    module React.Store.Searcher,
) where

import           Utils.PreludePlus



data Searcher = Searcher
      { _visible :: Bool
      , _input :: Text
      } deriving (Show, Generic)

makeLenses ''Searcher

instance Default Searcher where
    def = Searcher False def
