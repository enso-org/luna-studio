module JS.Key where

import           Common.Prelude

toKey :: Show a => a -> JSString
toKey = convert . intercalate "-" . toWords . map toLower . show where
    toWords [] = []
    toWords t  = let (w, suffix) = nextWord t in if null w then toWords suffix else w : toWords suffix
    nextWord = span isAlphaNum . dropWhile (not . isAlphaNum)
