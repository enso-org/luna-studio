{-# LANGUAGE LambdaCase #-}

module Utils.PreludePlus (
    module Utils.PreludePlus,
    module Prelude,
    module X
) where

import           Control.Applicative      as X
import           Control.Lens             as X
import           Control.Monad            as X (MonadPlus, join, mplus, mzero, unless, void, when, (<=<), (>=>))
import           Control.Monad.IO.Class   as X (MonadIO, liftIO)
import           Control.Monad.Trans      as X (MonadTrans, lift)
import           Data.Char                as X
import           Data.Default             as X
import           Data.Either              as X (isLeft, isRight)
import           Data.Foldable            as X (Foldable, foldlM, forM_, mapM_, sequenceA_, traverse_)
import           Data.Function            as X (on)
import           Data.JSString            as X (JSString)
import           Data.List                as X hiding (uncons)
import           Data.Maybe               as X
import           Data.Monoid              as X (Monoid, mappend, mconcat, mempty, (<>))
import           Data.String              as X (IsString (fromString))
import           Data.Text.Lazy           as X (Text)
import           Data.Traversable         as X (forM, mapM, sequenceA)
import           Data.Typeable            as X (Typeable)
import           Development.Placeholders as X
import           GHC.Exts                 as X (IsList, Item, fromList, fromListN, toList)
import           GHC.Generics             as X (Generic)
import           GHCJS.Types              as X (JSVal)
import           Prelude                  hiding (mapM, mapM_, print, putStr, putStrLn, (.))
import qualified Prelude
import           Prologue                 as X (toString)
import           Text.Show.Pretty         (ppShow)


print :: (MonadIO m, Show s) => s -> m ()
print    = liftIO . Prelude.print

printLn :: MonadIO m => m ()
printLn = putStrLn ""

putStr :: MonadIO m => String -> m ()
putStr   = liftIO . Prelude.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

prettyPrint :: (MonadIO m, Show s) => s -> m ()
prettyPrint = putStrLn . ppShow


infixr 9 .
(.) :: (Functor f) => (a -> b) -> f a -> f b
(.) = fmap

(.:)  :: (x -> y) -> (a -> b -> x) -> a -> b -> y
(.:)   = (.) . (.)

(.:.) :: (x -> y) -> (a -> b -> c -> x) -> a -> b -> c -> y
(.:.)  = (.) . (.) . (.)

(.::) :: (x -> y) -> (a -> b -> c -> d -> x) -> a -> b -> c -> d -> y
(.::)  = (.) . (.) . (.) . (.)


fromJustM :: Monad m => Maybe a -> m a
fromJustM Nothing  = fail "Prelude.fromJustM: Nothing"
fromJustM (Just x) = return x

whenLeft :: (Monad m) => Either a b -> (a -> m ()) -> m ()
whenLeft e f = case e of
    Left  v -> f v
    Right _ -> return ()

whenLeft' :: (Monad m) => Either a b -> m () -> m ()
whenLeft' e f = whenLeft e (const f)

whenRight :: (Monad m) => Either a b -> (b -> m ()) -> m ()
whenRight e f = case e of
    Left  _ -> return ()
    Right v -> f v

whenRight' :: (Monad m) => Either a b -> m () -> m ()
whenRight' e f = whenRight e $ const f

-- trenary operator
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
-- / trenary operator

($>) :: (Functor f) => a -> f b -> f b
($>) =  fmap . flip const

withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust = forM_

lift2 :: (Monad (t1 m), Monad m,
          MonadTrans t, MonadTrans t1)
      => m a -> t (t1 m) a
lift2 = lift . lift

lift3 :: (Monad (t1 (t2 m)), Monad (t2 m), Monad m,
          MonadTrans t, MonadTrans t1, MonadTrans t2)
      => m a -> t (t1 (t2 m)) a
lift3 = lift . lift2

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM predicate a b = do bool <- predicate
                       if bool then a else b

whenM :: Monad m => m Bool -> m () -> m ()
whenM predicate a = do
    bool <- predicate
    when bool a

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM predicate a = do
    bool <- predicate
    unless bool a

mjoin :: Monoid a => a -> [a] -> a
mjoin delim l = mconcat (intersperse delim l)

show' :: (Show a, IsString s) => a -> s
show' = fromString . Prelude.show

foldlDef :: (a -> a -> a) -> a -> [a] -> a
foldlDef f d = \case
    []     -> d
    (x:xs) -> foldl f x xs
