{-# LANGUAGE RankNTypes #-}
module System.Log.MLogger (
    module System.Log.MLogger,
    module X,
) where

import qualified Data.Maybe                  as Maybe
import qualified Data.String.Utils           as StringUtils
import           Prelude                     (fail)
import           Prologue                    hiding (fail, log)
import qualified System.Console.ANSI         as ANSI
import           System.IO                   (stderr)
import           System.Log.Logger           (Priority (..))
import qualified System.Log.Logger           as HSLogger

import           Control.Applicative
import           System.Log.MLogger.Location as X



type Logger    = forall m. MonadIO m => forall t. (t -> String -> m ()) -> t -> m ()

getLogger :: String -> Logger
getLogger name action msg = action msg name

log :: MonadIO m => Priority -> String -> String -> m ()
log pri msg name = liftIO $ do
  HSLogger.logM name pri (show pri <> " " <> msg)

trace, debug, info, warning, error, critical :: MonadIO m => String -> String -> m ()
trace    = log DEBUG
debug    = log DEBUG
info     = log INFO
warning  = log WARNING
error    = log ERROR
critical = log CRITICAL

criticalFail :: MonadIO m => String -> String -> m b
criticalFail msg name = do
    log CRITICAL msg name
    fail msg

setLevel :: MonadIO m => Priority -> String -> m ()
setLevel lvl name = liftIO $ HSLogger.updateGlobalLogger name (HSLogger.setLevel lvl)

setIntLevel :: Int -> String -> IO ()
setIntLevel lvl = setLevel nlvl where
    nlvl = case lvl of
        0 -> CRITICAL
        1 -> ERROR
        2 -> WARNING
        3 -> INFO
        _ -> DEBUG
