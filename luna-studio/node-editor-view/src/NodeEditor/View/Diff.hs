module NodeEditor.View.Diff where

import           Common.Prelude            hiding (get)
import           Control.Monad.Trans.State


type DiffT a = StateT (a, a)

runDiffT :: Monad m => DiffT a m r -> a -> a -> m r
runDiffT fun = curry $ evalStateT fun

diff :: Monad m => DiffT b m r -> Getting b a b -> DiffT a m r
diff fun getting = do
    (old, new) <- get
    lift $ runDiffT fun (new ^. getting) (old ^. getting)

diffApply :: (Eq a, Monad m) => (a -> DiffT a m ()) -> DiffT a m ()
diffApply apply = do
    (old, new) <- get
    when (new /= old) $ apply new
