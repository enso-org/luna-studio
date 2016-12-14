module JS.Tutorial
    ( shouldRunTutorial
    , showStep
    , closeOnboarding
    ) where

import           Luna.Studio.Prelude

foreign import javascript safe "localStorage.getItem('onboarding') != '1'" shouldRunTutorial :: IO Bool
foreign import javascript safe "require('Onboarding').show($1)" showStep :: Int -> IO ()
foreign import javascript safe "require('Onboarding').close()"  closeOnboarding :: IO ()
