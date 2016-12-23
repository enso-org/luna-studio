module Empire.Utils.TextResult where

import           Prologue

import           Empire.API.Data.DefaultValue (Value (..))


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

nodeValueToText :: Value -> Text
nodeValueToText w = convert $ case w of
    IntValue        v -> show v
    DoubleValue     v -> show v
    BoolValue       v -> show v
    StringValue     v -> showStr v

    IntMaybe        v -> show v
    DoubleMaybe     v -> show v
    BoolMaybe       v -> show v
    StringMaybe     v -> showMaybeStr v

    IntList         v -> "Vector ["  <> showLength v <> "]"
    DoubleList      v -> "Vector ["  <> showLength v <> "]"
    BoolList        v -> "Vector ["  <> showLength v <> "]"
    StringList      v -> "Vector ["  <> showLength v <> "]"

    DoublePairList  v -> "Vector2 [" <> showLength v <> "]"
    IntPairList     v -> "Vector2 [" <> showLength v <> "]"

    Histogram       v -> "Hist ["    <> showLength v <> "]"
    Image          {} -> "Image"

    StringStringMap v -> "Map [" <> showLength v <> "]"
    StringMaybeList v -> "Vector [" <> showLength v <> "]"

    Graphics        _ -> "Graphics"
    _                 -> "(unknown type)"
