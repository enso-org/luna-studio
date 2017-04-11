{-# LANGUAGE ExistentialQuantification #-}

module Empire.Data.AST where

import           Empire.Prelude hiding (cast)

import           Data.Typeable  (cast)

import           Luna.IR        (IR, IRBuilder, SomeExpr, SomeExprLink, evalIRBuilder', evalPassManager', runRegs, snapshot)

import           System.Log     (DropLogger, Logger, dropLogs)


type NodeRef       = SomeExpr
type EdgeRef       = SomeExprLink

data SomeASTException = forall e. Exception e => SomeASTException e

instance Show SomeASTException where
    show (SomeASTException e) = show e

instance Exception SomeASTException where
    displayException (SomeASTException e) = displayException e

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

data InvalidConnectionException = InvalidConnectionException deriving (Show)

instance Exception InvalidConnectionException where
    toException   = astExceptionToException
    fromException = astExceptionFromException

data NotInputEdgeException = NotInputEdgeException deriving (Show)

instance Exception NotInputEdgeException where
    toException   = astExceptionToException
    fromException = astExceptionFromException

data PortDoesNotExistException = PortDoesNotExistException deriving (Show)

instance Exception PortDoesNotExistException where
    toException   = astExceptionToException
    fromException = astExceptionFromException
