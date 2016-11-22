{-# LANGUAGE TypeFamilies #-}

module Reactive.Commands.Command where

import           Control.Lens.Internal.Zoom
import           Control.Monad.State
import           Utils.PreludePlus


newtype Command a b = Command { unCommand :: StateT a IO b }
                    deriving (Functor, Applicative, Monad, MonadIO, MonadState a)

type instance Zoomed (Command a) = Focusing IO

instance Zoom (Command s) (Command t) s t where
    zoom l (Command m) = Command (zoom l m)

command :: (a -> (IO (), a)) -> Command a ()
command f = do
    (action, st) <- gets f
    performIO action
    put st

pureCommand :: (a -> a) -> Command a ()
pureCommand = modify

ioCommand :: (a -> IO ()) -> Command a ()
ioCommand f = gets f >>= performIO

runCommand :: Command a b -> a -> IO (b, a)
runCommand cmd st = runStateT (unCommand cmd) st

execCommand :: Command a b -> a -> IO a
execCommand cmd st = snd <$> runCommand cmd st

performIO :: IO () -> Command a ()
performIO = liftIO
