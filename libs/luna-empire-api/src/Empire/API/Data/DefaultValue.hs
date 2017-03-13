module Empire.API.Data.DefaultValue where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Data.Text       (Text, pack)
import           Prologue        hiding (Text)

import qualified Graphics.API    as G


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

           | Lambda          String
           deriving (Generic, Eq, NFData, Show)

data PortDefault = Expression String | Constant Value deriving (Generic, Eq, NFData, Show)

instance Binary Value
instance Binary PortDefault

makePrisms ''Value
makePrisms ''PortDefault

stringify :: Value -> Text
stringify = pack . show
