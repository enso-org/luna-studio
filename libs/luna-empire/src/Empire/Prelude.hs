module Empire.Prelude (
      _1
    , _2
    , (.:.)
    , (.:)
    , (<>)
    , type (<>)
    , (<|>)
    , at
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
    , pathNameToString
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
    , module X
    ) where

import           Control.Applicative       ((<|>))
import           Control.Exception         (Exception (..), SomeException)
import           Control.Lens              as X (at, ix, makeLenses, preuse, use, uses, view, zoom, _1, _2)
import           Control.Lens.Operators    as X
import           Control.Lens.Prism        as X
import           Control.Monad             (when)
import           Control.Monad.Catch       (MonadThrow, throwM)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.State.Class (MonadState)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Default              (Default, def)
import           Data.Monoid               ((<>))
import           Data.Proxy                (Proxy (..))
import           Data.Typeable             (typeRep)
import           Prelude
import           Prologue                  (type (<>), notImplemented, typeRep', (.:), (.:.))

import qualified Data.Convert              as Convert
import qualified OCI.IR.Name.QualName      as IR

nameToString :: IR.Name -> String
nameToString = Convert.convert

pathNameToString :: IR.QualName -> String
pathNameToString = nameToString . Convert.convert

stringToName :: String -> IR.Name
stringToName = Convert.convert
