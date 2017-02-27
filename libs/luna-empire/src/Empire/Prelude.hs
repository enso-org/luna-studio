module Empire.Prelude (
      _1
    , _2
    , (.:.)
    , (.:)
    , (<>)
    , type (<>)
    , (<|>)
    , at
    , module Control.Lens.Operators
    , def
    , Default
    , SomeException
    , Exception(..)
    , ix
    , liftIO
    , makeLenses
    , MonadIO
    , MonadState
    , MonadThrow
    , MonadTrans(..)
    , nameToString
    , notImplemented
    , module Prelude
    , Proxy(..)
    , stringToName
    , throwM
    , typeRep
    , typeRep'
    , use
    , uses
    , view
    , when
    , zoom
    ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, Exception(..))
import Control.Lens (makeLenses, view, zoom, uses, use, at, _1, _2, ix)
import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default, def)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Typeable (typeRep)
import Prologue (notImplemented, typeRep', (.:), (.:.), type (<>))
import Prelude

import qualified Luna.IR.Name as IR
import qualified Data.Convert as Convert

nameToString :: IR.Name -> String
nameToString = Convert.convert

stringToName :: String -> IR.Name
stringToName = Convert.convert
