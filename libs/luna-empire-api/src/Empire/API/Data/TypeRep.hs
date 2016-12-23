module Empire.API.Data.TypeRep where

import Prologue    hiding (intercalate, TypeRep)

import Data.Binary (Binary)
import Data.List   (intercalate)

data TypeRep = TCons String    [TypeRep]
             | TVar  String
             | TLam  [TypeRep] TypeRep
             | TStar
             | TBlank
             | TAcc  String TypeRep
             deriving (Show, Eq, Generic)

instance Binary TypeRep

instance ToString TypeRep where
    toString = toString' False False where
        parenIf cond expr = if cond then "(" <> expr <> ")" else expr

        toString' parenCons _ (TCons name args) = case name of
            "List" -> "[" <> concat (toString' False False <$> args) <> "..]"
            _      -> let reps = toString' True True <$> args
                          par  = parenCons && (not . null $ reps)
                      in parenIf par $ unwords (name : reps)
        toString' _ parenLam (TLam args out) = parenIf parenLam $ intercalate " -> " reps <> " -> " <> outRep where
            reps   = toString' False True <$> args
            outRep = toString' False False out
        toString' _ _ (TVar n) = n
        toString' _ _ TStar = "*"
        toString' _ _ TBlank = ""
        toString' _ _ (TAcc n t) = toString' True True t <> "." <> n

instance Convertible TypeRep Text where
    convert = convert . toString ; {-# INLINE convert #-}
