module Empire.Utils.TextResult where

import qualified Data.Text                   as Text
import           Empire.Prelude

import           Empire.API.Data.PortDefault (Value (..))


showLength :: [a] -> String
showLength list = show len <> if exceed then " or more" else "" where
    (len, exceed) = limitedLen list

maxLen = 10000000

limitedLen :: [a] -> (Integer, Bool)
limitedLen = limitedLen' 0 where
    limitedLen' :: Integer -> [a] -> (Integer, Bool)
    limitedLen' acc []     = (acc, False)
    limitedLen' acc (x:xs) = if acc < maxLen
                                then limitedLen' (acc + 1) xs
                                else (acc, True)

showStr :: String -> String
showStr v = "\"" <> (if length v > 10 then take 10 v <> "..." else v) <> "\""

showMaybeStr :: Maybe String -> String
showMaybeStr Nothing  = show (Nothing :: Maybe String)
showMaybeStr (Just s) = show (Just $ showStr s)
