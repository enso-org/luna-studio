module LunaStudio.Data.TypeRep where

import Prologue hiding (Text, TypeRep, intercalate)

import Control.DeepSeq  (NFData)
import Control.Lens     (makePrisms)
import Data.Aeson.Types (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary      (Binary)
import Data.Hashable    (Hashable)
import Data.Text        (Text)


data TypeRep
    = TCons String  [TypeRep]
    | TVar  String
    | TLam  TypeRep TypeRep
    | TStar
    | TBlank
    | TAcc  String TypeRep
    deriving (Eq, Generic, Show)

makePrisms ''TypeRep

instance NFData TypeRep
instance Binary TypeRep
instance Hashable TypeRep
instance FromJSON TypeRep
instance FromJSONKey TypeRep
instance ToJSON TypeRep
instance ToJSONKey TypeRep

instance ToString TypeRep where
    toString = toString' False False where
        parenIf cond expr = if cond then "(" <> expr <> ")" else expr

        toString' parenCons _ (TCons name args) = case name of
            "List" -> "[" <> concat (toString' False False <$> args) <> "..]"
            _      -> let reps = toString' True True <$> args
                          par  = parenCons && (not . null $ reps)
                      in parenIf par $ unwords (name : reps)
        toString' _ parenLam (TLam arg out)
            = parenIf parenLam $ argRep <> " -> " <> outRep where
                argRep = toString' False True  arg
                outRep = toString' False False out
        toString' _ _ (TVar n) = n
        toString' _ _ TStar = "*"
        toString' _ _ TBlank = ""
        toString' _ _ (TAcc n t) = toString' True True t <> "." <> n

instance Convertible TypeRep Text where
    convert = convert . toString ; {-# INLINE convert #-}


data ConstructorRep = ConstructorRep
    { constructor :: Text
    , fields      :: [ConstructorRep]
    } deriving (Eq, Generic, Show)

instance NFData ConstructorRep
instance ToJSON ConstructorRep

toConstructorRep :: TypeRep -> Maybe ConstructorRep
toConstructorRep (TCons c f)
    = ConstructorRep (convert c) <$> mapM toConstructorRep f
toConstructorRep _           = Nothing

matchTypes :: TypeRep -> TypeRep -> Bool
matchTypes TStar _ = True
matchTypes _ TStar = True
matchTypes t1 t2   = t1 == t2
