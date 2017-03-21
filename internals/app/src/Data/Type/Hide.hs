{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Type.Hide where

import           Prelude       hiding (lookup)
import           Unsafe.Coerce

class HideType a c where
    hideType   :: a -> c
    revealType :: c -> a

data Simple where
    Simple :: a -> Simple

instance Show Simple where
    show _ = "Hidden"

instance HideType a Simple where
    hideType              = Simple
    revealType (Simple x) = unsafeCoerce x
