{-# LANGUAGE OverloadedStrings #-}
module JS.Searcher where --TODO[basegl] Remove

import           Common.Prelude
import qualified JS.Mount       as Mount
import qualified JS.UI          as UI


searcherId :: JSString
searcherId = Mount.prefix "focus-searcher"

selection :: MonadIO m => m (Int, Int)
selection = liftIO $ (,) <$> selectionStart searcherId <*> selectionEnd searcherId

foreign import javascript safe "document.getElementById($1).selectionStart" selectionStart :: JSString -> IO Int
foreign import javascript safe "document.getElementById($1).selectionEnd"   selectionEnd   :: JSString -> IO Int
foreign import javascript safe "document.getElementById($1).setSelectionRange($2, $3)" setSelection' :: JSString -> Int -> Int -> IO ()

setSelection :: MonadIO m => Int -> Int -> m ()
setSelection = liftIO .: setSelection' searcherId
