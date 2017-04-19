{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Node.Editor.Data.CtxDynamic where

import           Data.Dynamic
import           Luna.Prelude
import           Unsafe.Coerce

data CtxDynamic ctx where
    CtxDynamic :: ctx a => TypeRep -> a -> CtxDynamic ctx


toCtxDynamic :: (ctx a, Typeable a) => a -> CtxDynamic ctx
toCtxDynamic a = CtxDynamic (typeOf a) a

fromCtxDynamic :: forall ctx a. (ctx a, Typeable a) => CtxDynamic ctx -> Maybe a
fromCtxDynamic (CtxDynamic t el) = if t == typeOf (undefined :: a) then Just $ unsafeCoerce el
                                                                   else Nothing

withCtxDynamic :: (forall a. ctx a => a -> b) -> CtxDynamic ctx -> b
withCtxDynamic f (CtxDynamic _ a) = f a
