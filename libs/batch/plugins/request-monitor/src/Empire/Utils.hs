module Empire.Utils where

import           Prologue

import qualified Text.Show.Pretty as Pretty


lastPart :: Eq a => a -> [a] -> [a]
lastPart = lastPartIntern []

lastPartIntern :: Eq a => [a] -> a -> [a] -> [a]
lastPartIntern _      b (a:as) | a == b = lastPartIntern [] b as
lastPartIntern buffer _ []              = reverse buffer
lastPartIntern buffer b (a:as)          = lastPartIntern (a:buffer) b as

display :: Show a => Bool -> a -> String
display True  = Pretty.ppShow
display False = show
