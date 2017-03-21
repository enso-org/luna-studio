{-# LANGUAGE OverloadedStrings #-}
module JS.Searcher where

import qualified JS.Config           as Config
-- import qualified JS.UI               as UI
import           Luna.Studio.Prelude


searcherId :: JSString
searcherId = undefined--Config.prefix "focus-searcher"

selection :: MonadIO m => m (Int, Int)
selection = undefined --liftIO $ (,) <$> selectionStart searcherId <*> selectionEnd searcherId

-- foreign import javascript safe "document.getElementById($1).selectionStart" selectionStart :: JSString -> IO Int
-- foreign import javascript safe "document.getElementById($1).selectionEnd"   selectionEnd   :: JSString -> IO Int

focus :: IO ()
focus = undefined --UI.focus searcherId
