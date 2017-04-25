{-# LANGUAGE RankNTypes #-}
module System.Log.MLogger (
    module System.Log.MLogger,
    module X,
) where

import           Prelude                     (fail)
import qualified Data.Maybe                  as Maybe
import qualified Data.String.Utils           as StringUtils
import           Prologue                    hiding (log, fail)
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
    let sgr   = case pri of
                   DEBUG       -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Magenta]
                   INFO        -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green  ]
                   WARNING     -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow ]
                   ERROR       -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red    ]
                   CRITICAL    -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red    ]

        componentsOfName :: String -> [String]
        componentsOfName n =
          let joinComp [] _ = []
              joinComp (x:xs) [] = x : joinComp xs x
              joinComp (x:xs) accum =
                  let newlevel = accum ++ "." ++ x in
                      newlevel : joinComp xs newlevel
              in
              HSLogger.rootLoggerName : joinComp (StringUtils.split "." n) []

        parentLoggers [] = return []
        parentLoggers n = do
            let pname = (head . drop 1 . reverse . componentsOfName) n
            parent <- HSLogger.getLogger pname
            next <- parentLoggers pname
            return (parent : next)

        getLoggerPriority :: String -> IO Priority
        getLoggerPriority n = do
            l <- HSLogger.getLogger n
            pl <- parentLoggers n
            case Maybe.mapMaybe HSLogger.getLevel (l : pl) of
                [] -> return HSLogger.DEBUG
                (x:_) -> return x

    lpri <- getLoggerPriority name
    when (pri >= lpri) $ ANSI.hSetSGR stderr sgr
    HSLogger.logM name pri msg
    when (pri >= lpri) $ ANSI.hSetSGR stderr []

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
