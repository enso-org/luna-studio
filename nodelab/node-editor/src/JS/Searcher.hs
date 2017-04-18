{-# LANGUAGE OverloadedStrings #-}
module JS.Searcher where

import qualified JS.Config           as Config
import qualified JS.UI               as UI
import           Luna.Prelude


searcherId :: JSString
searcherId = Config.prefix "focus-searcher"

selection :: MonadIO m => m (Int, Int)
selection = liftIO $ (,) <$> selectionStart searcherId <*> selectionEnd searcherId

foreign import javascript safe "document.getElementById($1).selectionStart" selectionStart :: JSString -> IO Int
foreign import javascript safe "document.getElementById($1).selectionEnd"   selectionEnd   :: JSString -> IO Int

focus :: IO ()
focus = UI.focus searcherId
