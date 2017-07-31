{-# LANGUAGE TypeFamilies #-}
module Common.Action.Command where

import           Common.Prelude             hiding (gets)
import           Control.Lens.Internal.Zoom
import           Control.Monad.State


newtype Command a b = Command { unCommand :: StateT a IO b }
                    deriving (Functor, Applicative, Monad, MonadIO, MonadState a)

type instance Zoomed (Command a) = Focusing IO

instance Zoom (Command s) (Command t) s t where
    zoom l (Command m) = Command (zoom l m)

command :: (a -> (IO (), a)) -> Command a ()
command f = do
    (action, st) <- gets f
    liftIO action
    put st

runCommand :: Command a b -> a -> IO (b, a)
runCommand cmd = runStateT $ unCommand cmd

execCommand :: Command a b -> a -> IO a
execCommand cmd st = snd <$> runCommand cmd st
