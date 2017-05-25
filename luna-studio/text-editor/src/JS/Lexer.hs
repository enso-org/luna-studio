{-# LANGUAGE JavaScriptFFI #-}

module JS.Lexer
    ( installLexer
    ) where


import           Common.Prelude
import qualified Data.Text.Span               as Span
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal                (ToJSVal (toJSVal))
import           GHCJS.Marshal.Pure           (pFromJSVal, pToJSVal)
import           JavaScript.Array             (JSArray)
import qualified JavaScript.Array             as JSArray
import qualified Luna.Syntax.Text.Lexer       as Lexer
import           Luna.Syntax.Text.Lexer.Class (Token)
import qualified Luna.Syntax.Text.Lexer.Class as Token


foreign import javascript safe "atomCallbackTextEditor.setLexer($1)"
    setLexer' :: Callback (JSVal -> IO JSVal) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.unsetLexer()"
    unsetLexer :: IO ()

foreign import javascript safe "{length: $1, tags: $2}"
    exportToken' :: Int -> JSArray -> JSVal

exportToken :: Token (Lexer.Symbol JSString) -> [JSVal]
exportToken token =
    [ exportToken' (fromIntegral $ unwrap $ token ^. Token.span . Span.length)
                   (JSArray.fromList $ map pToJSVal $ Lexer.tags $ Token.untoken token)
    , exportToken' (fromIntegral $ unwrap $ token ^. Token.span . Span.offset)
                   (JSArray.fromList [])

    ]

instance ToJSVal (Lexer.SymbolStream JSString) where
    toJSVal (Lexer.Stream tokens) = toJSValListOf $ concatMap exportToken tokens

setLexer :: (String -> IO (Lexer.SymbolStream JSString)) -> IO (IO ())
setLexer lexer = do
    wrappedCallback <- syncCallback1' $ toJSVal <=< lexer . pFromJSVal
    setLexer' wrappedCallback
    return $ unsetLexer >> releaseCallback wrappedCallback

installLexer :: IO (IO ())
installLexer = setLexer $ return . Lexer.runLexer
