module Empire.API.Data.DefaultValue where

import           Data.Binary          (Binary)
import           Prologue

import qualified Graphics.API      as G


data Value = IntValue        Int
           | DoubleValue     Double
           | RationalValue   Rational
           | BoolValue       Bool
           | StringValue     String

           | IntList         [Int]
           | DoubleList      [Double]
           | BoolList        [Bool]
           | StringList      [String]

           | IntMaybe        (Maybe Int)
           | DoubleMaybe     (Maybe Double)
           | BoolMaybe       (Maybe Bool)
           | StringMaybe     (Maybe String)

           | DoublePairList  [(Double, Double)]  -- TODO: obsolete, remove
           | IntPairList     [(Int, Int)]        -- TODO: obsolete, remove
           | Histogram       [(Int, Int)]        -- TODO: obsolete, remove (after changing histogram)
           | DataFrame       [(String, [Value])] -- TODO: obsolete, remove

           | Image           String Double Double

           | StringMaybeList [Maybe String]
           | StringStringMap [(String, String)]

           | Graphics        G.Graphics
           | Lambda          String
           deriving (Generic, Show, Eq)

data PortDefault = Expression String | Constant Value deriving (Generic, Show, Eq)

instance Binary Value
instance Binary PortDefault

makePrisms ''Value
makePrisms ''PortDefault

stringify :: Value -> Text
stringify = convert . show
