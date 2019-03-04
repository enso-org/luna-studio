{-# LANGUAGE JavaScriptFFI #-}

module JS.SearcherEngine where

import Common.Prelude
import GHCJS.Marshal.Pure            (pToJSVal)
import LunaStudio.Data.Searcher.Hint (SearcherHint, prefix, documentation)
import Searcher.Engine.Data.Database (SearcherData (fixedScore, text))

newtype Match = Match [Int] deriving (Show, Eq, Generic)
makeWrapped ''Match
instance NFData Match

data Result a = Result
    { _hint :: a
    , _score :: Double
    , _match :: Match
    } deriving (Functor, Show, Eq, Generic)
makeLenses ''Result

instance NFData a => NFData (Result a)

instance SearcherData a => SearcherData (Result a) where
    text       = hint . text
    fixedScore = hint . fixedScore

instance SearcherHint a => SearcherHint (Result a) where
    prefix        = hint . prefix
    documentation = hint . documentation

newtype Database = Database JSVal deriving Generic
makeWrapped ''Database
instance NFData Database
instance Default Database where
    def = wrap $ createEmptyDatabase'

foreign import javascript safe "new window.searcherEngine.Database($1)"
    createDatabase' :: JSVal -> JSVal

createDatabase :: [(Int, Text)] -> IO Database
createDatabase items = wrap . createDatabase' <$> toJSVal items

foreign import javascript safe "new window.searcherEngine.Database()"
    createEmptyDatabase' :: JSVal

foreign import javascript safe "$1.query($2)"
    query' :: JSVal -> JSVal -> JSVal

query :: MonadIO m => Database -> Text -> m [Result Int]
query db q = do
    let bareResults = query' (unwrap db) (pToJSVal q)
    Just (tResults :: [([Int], [Int], Double)]) <- liftIO $ fromJSVal bareResults
    let mkResult match score ix = Result ix score (Match match)
        processResultGroup (ixes, match, score) = mkResult match score <$> ixes
        results = tResults >>= processResultGroup
    pure results
