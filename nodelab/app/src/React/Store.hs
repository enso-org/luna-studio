module React.Store where

import           React.Flux
import           Utils.PreludePlus         as P

import           Reactive.Commands.Command (Command)


modify :: Typeable s => (s -> IO (s, r)) -> ReactStore s -> Command a r
modify = liftIO .: flip modifyStore

modify_ :: Typeable s => (s -> IO s) -> ReactStore s -> Command a ()
modify_ action = modify $ \s -> do
    s' <- action s
    return (s', ())


modifyIf :: Typeable s
         => (s -> Bool)
            -> (s -> IO (s, r))
            -> (s -> IO r)
         -> ReactStore s
         -> Command a r
modifyIf cond actionTrue actionFalse store = liftIO $ modifyStoreIf store cond actionTrue actionFalse

inside :: (p -> Command a r) -> ReactStore p -> Command a r
inside action parentRef = action =<< get parentRef

get :: ReactStore p -> Command s p
get = liftIO . getStoreData

use :: Getting r s r -> ReactStore s -> Command state r
use getter store = P.view getter <$> get store
