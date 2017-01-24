{-# LANGUAGE ExistentialQuantification #-}

module Empire.Data.AST where

import           Empire.Prelude

import           Control.Monad.State (StateT)
import           Data.Typeable       (cast)
import           Empire.Data.Layers  (attachEmpireLayers, registerEmpireLayers)

import           Luna.IR            (AnyExpr, AnyExprLink, IR, IRBuilder,
                                     evalIRBuilder', evalPassManager', snapshot,
                                     runRegs)
import qualified Luna.Pass.Manager  as Pass (State)
import qualified Luna.Pass.Manager  as PassManager (get)

import           System.Log         (Logger, DropLogger, dropLogs)


type NodeRef       = AnyExpr
type EdgeRef       = AnyExprLink

data SomeASTException = forall e. Exception e => SomeASTException e

instance Show SomeASTException where
    show (SomeASTException e) = show e

instance Exception SomeASTException

astExceptionToException :: Exception e => e -> SomeException
astExceptionToException = toException . SomeASTException

astExceptionFromException :: Exception e => SomeException -> Maybe e
astExceptionFromException x = do
    SomeASTException a <- fromException x
    cast a

data NotUnifyException = NotUnifyException NodeRef
    deriving (Show)

instance Exception NotUnifyException where
    toException = astExceptionToException
    fromException = astExceptionFromException

data NotLambdaException = NotLambdaException NodeRef
    deriving (Show)

instance Exception NotLambdaException where
    toException = astExceptionToException
    fromException = astExceptionFromException

data NotAppException = NotAppException NodeRef
    deriving (Show)

instance Exception NotAppException where
    toException = astExceptionToException
    fromException = astExceptionFromException
