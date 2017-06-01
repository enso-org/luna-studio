{-# LANGUAGE JavaScriptFFI #-}

module JS.Lexer
    ( installLexer
    ) where

import           Common.Prelude
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure     (pFromJSVal, pToJSVal)
import           JavaScript.Array       (JSArray)
import qualified JavaScript.Array       as JSArray
import           Luna.Syntax.Text.Lexer (LexerGUIToken)
import qualified Luna.Syntax.Text.Lexer as Lexer


foreign import javascript safe "atomCallbackTextEditor.setLexer($1)"
    setLexer' :: Callback (JSVal -> IO JSVal) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.unsetLexer()"
    unsetLexer :: IO ()

foreign import javascript safe "{length: $1, tags: $2}"
    exportToken' :: Int -> JSArray -> JSVal

exportToken :: LexerGUIToken String -> [JSVal]
exportToken token =
    [ exportToken' (fromIntegral $ unwrap $ token ^. Lexer.guiSpan)
                   (JSArray.fromList $ map pToJSVal $ Lexer.getTags token)
    , exportToken' (fromIntegral $ unwrap $ token ^. Lexer.guiOffset)
                   (JSArray.fromList [])
    ]

setLexer :: (String -> IO [LexerGUIToken String]) -> IO (IO ())
setLexer lexer = do
    wrappedCallback <- syncCallback1' $ toJSValListOf . concatMap exportToken <=< lexer . pFromJSVal
    setLexer' wrappedCallback
    return $ unsetLexer >> releaseCallback wrappedCallback

installLexer :: IO (IO ())
installLexer = setLexer $ return . Lexer.runGUILexer
