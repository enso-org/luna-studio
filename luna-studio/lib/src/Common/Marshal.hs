module Common.Marshal where

import Common.Prelude

import System.IO.Unsafe    (unsafePerformIO)

unsafeToJSVal :: ToJSVal a => a -> JSVal
unsafeToJSVal = unsafePerformIO . toJSVal

