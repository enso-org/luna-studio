{-# LANGUAGE ExistentialQuantification #-}

module Empire.Data.AST where

import           Empire.Prelude

import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable          (cast)

import           Luna.IR              (SomeTerm, SomeLink)
import           LunaStudio.Data.Node (NodeId)
import           LunaStudio.Data.Port (AnyPortId, OutPortId)


type NodeRef       = SomeTerm
type EdgeRef       = SomeLink

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

data NotInputEdgeException = NotInputEdgeException {
      _expected :: NodeId
    , _received :: NodeId
    } deriving (Show)

makeLenses ''NotInputEdgeException

instance Exception NotInputEdgeException where
    toException   = astExceptionToException
    fromException = astExceptionFromException
    displayException e = "Exception: " <> show (e ^. received)
                        <> " is not an input sidebar ID ("
                        <> show (e ^. expected) <> ")"

data PortDoesNotExistException = PortDoesNotExistException OutPortId deriving (Show)

instance Exception PortDoesNotExistException where
    toException   = astExceptionToException
    fromException = astExceptionFromException

data ConnectionException = ConnectionException {
      _srcNodeId   :: NodeId
    , _srcNodeCode :: Text
    , _srcPort     :: OutPortId
    , _dstNodeId   :: NodeId
    , _dstNodeCode :: Text
    , _dstPort     :: AnyPortId
    , _innerExc    :: SomeASTException
    } deriving Show

makeLenses ''ConnectionException

instance Exception ConnectionException where
    toException        = astExceptionToException
    fromException      = astExceptionFromException
    displayException e = "Exception occurred during connecting port "
                      <> show (e ^. srcPort) <> " of \""
                      <> Text.unpack (e ^. srcNodeCode) <> "\" (" <> show (e ^. srcNodeId) <> ") to port "
                      <> show (e ^. dstPort) <> " of \""
                      <> Text.unpack (e ^. dstNodeCode) <> "\" (" <> show (e ^. dstNodeId) <> "): "
                      <> displayException (e ^. innerExc)
