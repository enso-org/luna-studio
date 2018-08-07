{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module ZMQ.Bus.RPC.Types (
    buildName,
    packValue,
    unpackValue,
    FunctionName,
    Request(..),
    requestMethod,
    arguments,
    Response(..),
    Result(..),
    Value(..),
    Serializable
)where

import           Control.Error        (ExceptT, throwE)
import           Control.Exception    (SomeException)
import           Control.Monad.Catch  (MonadCatch, catch)
import           Data.Binary
import           Data.ByteString.Lazy as BS (ByteString)
import           Data.List            as L
import           Data.Typeable

import           Prologue      hiding (Context, TypeRep, typeRep)


type FunctionName = String


type Serializable val = (Binary val, Typeable val)


data Request = Request { _requestMethod :: FunctionName
                       , _arguments     :: Value
                       } deriving (Show, Generic)


data Response = Response { responseMethod :: FunctionName
                         , result         :: Result
                         , messages       :: [Value]
                         } deriving (Show, Generic)

data Result = ErrorResult  { errorMsg :: String }
            | Status       { retVal :: Value  }
            deriving (Show, Generic)


data Value = Value { typeName  :: String
                   , protocol  :: String
                   , dataBytes :: ByteString
                   } deriving (Show, Generic)


instance Binary Request
instance Binary Response
instance Binary Result
instance Binary Value


makeLenses ''Request


packValue :: forall a. (Binary a, Typeable a) => a -> Value
packValue input = Value ({-show $ typeOf input-} buildName (Proxy :: Proxy a)) "bin" (encode input)


unpackValue :: forall a m. (Binary a, Typeable a, MonadCatch m) => Value -> ExceptT String m a
unpackValue (Value tname "bin" bytes) = do
    let expectedTname = buildName (Proxy :: Proxy a)
        handler       :: SomeException -> ExceptT String m a
        handler       = const $ throwE "Not enough bytes; received message is corrupted"
    if
        expectedTname == tname
          then (return $! decode bytes) `catch` handler
          else throwE $ "Could not match expected type `" <> expectedTname <> "'\n"
                    <> "             with actual type `" <> tname         <> "'"
unpackValue val = throwE $ "Not supported protocol " <> protocol val




buildName :: Typeable a => Proxy a -> String
buildName = recurBuildName . typeRep


recurBuildName :: TypeRep -> String
recurBuildName rep = buildQualifiedName con <> rArgs
    where con  = typeRepTyCon rep
          args = typeRepArgs  rep
          rArgs = if null args then "" else "[" <> L.intercalate ", " (map recurBuildName args) <> "]"


buildQualifiedName :: TyCon -> String
buildQualifiedName tyCon = L.intercalate "." fullPath
    where fullPath = [tyConModule, tyConName] <*> [tyCon]
