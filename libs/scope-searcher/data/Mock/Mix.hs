{-# LANGUAGE OverloadedStrings #-}

module Mock.Mix where

import qualified Data.Map                as Map
import           Text.ScopeSearcher.Item

items = _items group

mkElement name = (name, Element ())
mkGroup name list = (name, Group list ())

group  = Group (Map.fromList  [
         mkElement "id"
       , mkElement "const"
       , mkElement "app"
       , mkElement "comp"
       , mkElement "flip"
       , mkElement "empty"
       , mkElement "singleton"
       , mkElement "switch"
       , mkElement "readFile"
       , mkElement "mean"
       , mkElement "differences"
       , mkElement "histogram"
       , mkElement "primes"
-- purely for testing
       , mkElement "filter"
       , mkElement "moaBurger"
       , mkElement "MapOverAny"
-- !purely for testing
       , mkGroup "List"   $ Map.fromList [ mkElement "+"
                                         , mkElement "append"
                                         , mkElement "prepend"
                                         , mkElement "length"
                                         , mkElement "reverse"
                                         , mkElement "take"
                                         , mkElement "drop"
                                         , mkElement "sort"

                                         , mkElement "fold"
                                         , mkElement "map"
                                         , mkElement "zip"
                                         , mkElement "filter"
                                         ]
       , mkGroup "Int"    $ Map.fromList [ mkElement "=="
                                         , mkElement "/="
                                         , mkElement "<"
                                         , mkElement "<="
                                         , mkElement ">"
                                         , mkElement ">="
                                         , mkElement "min"
                                         , mkElement "max"

                                         , mkElement "+"
                                         , mkElement "*"
                                         , mkElement "-"
                                         , mkElement "/"
                                         , mkElement "%"
                                         , mkElement "^"

                                         , mkElement "negate"
                                         , mkElement "abs"
                                         , mkElement "signum"

                                         , mkElement "pred"
                                         , mkElement "succ"
                                         , mkElement "even"
                                         , mkElement "odd"

                                         , mkElement "gcd"
                                         , mkElement "lcm"

                                         , mkElement "times"
                                         , mkElement "upto"

                                         , mkElement "toDouble"
                                         , mkElement "toString"
                                         ]
       , mkGroup "Double" $ Map.fromList [ mkElement "=="
                                         , mkElement "/="
                                         , mkElement "<"
                                         , mkElement "<="
                                         , mkElement ">"
                                         , mkElement ">="
                                         , mkElement "min"
                                         , mkElement "max"

                                         , mkElement "+"
                                         , mkElement "*"
                                         , mkElement "-"
                                         , mkElement "/"
                                         , mkElement "**"

                                         , mkElement "negate"
                                         , mkElement "abs"
                                         , mkElement "signum"

                                         , mkElement "round"
                                         , mkElement "ceiling"
                                         , mkElement "floor"

                                         , mkElement "exp"
                                         , mkElement "log"
                                         , mkElement "sqrt"

                                         , mkElement "sin"
                                         , mkElement "cos"
                                         , mkElement "tan"
                                         , mkElement "asin"
                                         , mkElement "acos"
                                         , mkElement "atan"
                                         , mkElement "sinh"
                                         , mkElement "cosh"
                                         , mkElement "tanh"
                                         , mkElement "asinh"
                                         , mkElement "acosh"
                                         , mkElement "atanh"

                                         , mkElement "toString"
                                         ]
       , mkGroup "Bool"   $ Map.fromList [ mkElement "=="
                                         , mkElement "/="
                                         , mkElement "<"
                                         , mkElement "<="
                                         , mkElement ">"
                                         , mkElement ">="
                                         , mkElement "min"
                                         , mkElement "max"

                                         , mkElement "&&"
                                         , mkElement "||"
                                         , mkElement "not"

                                         , mkElement "toString"
                                         ]
       , mkGroup "String" $ Map.fromList [ mkElement "=="
                                         , mkElement "/="
                                         , mkElement "<"
                                         , mkElement "<="
                                         , mkElement ">"
                                         , mkElement ">="
                                         , mkElement "min"
                                         , mkElement "max"
                                         , mkElement "+"
                                         , mkElement "length"
                                         , mkElement "reverse"
                                         , mkElement "take"
                                         , mkElement "drop"
                                         , mkElement "words"
                                         , mkElement "lines"
                                         , mkElement "join"
                                         , mkElement "toString"
                                         ]
       ]) ()
