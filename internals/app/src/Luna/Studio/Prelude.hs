{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Luna.Studio.Prelude (
    module Luna.Studio.Prelude,
    module Prelude,
    module X
) where


import           Control.Applicative           as X
import           Control.Lens                  as X
import           Control.Monad                 as X (MonadPlus, join, mplus, mzero, unless, void, when, (<=<), (>=>))
import           Control.Monad.IO.Class        as X (MonadIO, liftIO)
import           Control.Monad.Trans           as X (MonadTrans, lift)
import qualified Control.Monad.Trans.Maybe     as MaybeT
import           Data.Char                     as X
import           Data.Default                  as X
import           Data.Either                   as X (isLeft, isRight)
import           Data.Foldable                 as X (Foldable, foldlM, forM_, mapM_, sequenceA_, traverse_)
import           Data.Function                 as X (on)
import           Data.JSString                 as X (JSString)
import           Data.List                     as X hiding (uncons, (++))
import           Data.Maybe                    as X
import           Data.Monoid                   as X (Monoid, mappend, mconcat, mempty, (<>))
import           Data.String                   as X (IsString (fromString))
import           Data.Text                     as X (Text)
import           Data.Traversable              as X (forM, mapM, sequenceA)
import           Data.Typeable                 as X (Typeable)
import           Development.Placeholders      as X
import           GHC.Exts                      as X (IsList, Item, fromList, fromListN, toList)
import           GHC.Generics                  as X (Generic)
import           GHCJS.Marshal                 as X (FromJSVal (..), ToJSVal (..))
import           GHCJS.Types                   as X (JSVal)
import           Luna.Studio.Prelude.Instances ()
import           Prelude                       hiding (curry, print, putStr, putStrLn, uncurry, (++), (.))
import           Prologue                      as X (NFData, convert, curry, fmap1, fmap2, fmap3, fmap4, fmap5, fmap6, fmap7, fmap8, fmap9,
                                                     foldlDef, fromJustM, ifElseId, ifM, lift2, lift3, pprint, putStr, show', switch,
                                                     toString, uncurry, unlessM, whenLeft, whenLeft', whenM, whenRight, whenRight',
                                                     withJust, ($>), (++), (.), (.:), (.:.), (.::), (.::.), (.:::), (.:::.), (.::::),
                                                     (.::::.), (<<∘>>), (<<∘∘>>), (<∘>), (<∘∘>), (<∘∘∘>), (<∘∘∘∘>), (<∘∘∘∘∘>))


foreign import javascript safe "console.log($1)" consoleLog :: JSString -> IO ()

print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . show

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . consoleLog . convert

printLn :: MonadIO m => m ()
printLn = putStrLn def

mjoin :: Monoid a => a -> [a] -> a
mjoin delim l = mconcat (intersperse delim l)

jsShow :: Show a => a -> JSString
jsShow = convert . show

withJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
withJustM mMayVal action = do
    mayVal <- mMayVal
    withJust mayVal action

keyed :: [a] -> [(Int, a)]
keyed = zip [0..]

forKeyed_ :: Monad m => [a] -> ((Int, a) -> m ()) -> m ()
forKeyed_ = forM_ . keyed

-- | From Control.Errors. Analogous to 'Just' and equivalent to 'return'
just :: (Monad m) => a -> MaybeT.MaybeT m a
just a = MaybeT.MaybeT (return (Just a))

-- | From Control.Errors. Analogous to 'Nothing' and equivalent to 'mzero'
nothing :: (Monad m) => MaybeT.MaybeT m a
nothing = MaybeT.MaybeT (return Nothing)