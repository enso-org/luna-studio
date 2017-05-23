{-# LANGUAGE JavaScriptFFI #-}

module JS.Lexer
    ( installLexer
    ) where


import           Common.Prelude
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure     (pFromJSVal, PToJSVal(pToJSVal))
import qualified Luna.Syntax.Text.Lexer as Lexer


foreign import javascript safe "atomCallbackTextEditor.setLexer($1)"
    setLexer' :: Callback (JSVal -> IO JSVal) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.unsetLexer()"
    unsetLexer :: IO ()

instance PToJSVal (Lexer.Stream JSString) where
    pToJSVal = $notImplemented

setLexer :: (String -> IO (Lexer.Stream JSString)) -> IO (IO ())
setLexer lexer = do
    wrappedCallback <- syncCallback1' $ fmap pToJSVal . lexer . pFromJSVal
    setLexer' wrappedCallback
    return $ unsetLexer >> releaseCallback wrappedCallback

installLexer :: IO (IO ())
installLexer = setLexer $ return . Lexer.runLexer
