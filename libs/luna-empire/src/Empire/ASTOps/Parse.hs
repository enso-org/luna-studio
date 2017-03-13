{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.ASTOps.Parse (
    parseExpr
  , parsePortDefault
  ) where

import           Empire.Prelude
import           Data.Convert

import           Data.List                    (partition)
import qualified Data.Text                    as Text

import           Empire.Data.AST              (NodeRef, astExceptionToException,
                                               astExceptionFromException)
import           Empire.ASTOps.Builder        (lams)
import           Empire.ASTOp                 (ASTOp)

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))

import qualified Luna.IR as IR
import qualified Luna.Syntax.Text.Parser.Parser         as Parser
import qualified Luna.Syntax.Text.Parser.Parsing        as Parsing

data ParserException e = ParserException e
    deriving (Show)

instance Exception e => Exception (ParserException e) where
    toException = astExceptionToException
    fromException = astExceptionFromException
    displayException (ParserException e) = "ParserException (" ++ displayException e ++ ")"

parseExpr :: ASTOp m => String -> m (Maybe Text.Text, NodeRef)
parseExpr s = do
  lamRes <- tryParseLambda s
  parsed <- pure $ Parsing.runParser Parsing.expr s
  case lamRes of
      (name, Just l)  -> return (name, l)
      _               -> case parsed of
          Right (Parser.IRB x) -> do
              x' <- x
              return (Nothing, x')
          Left err -> throwM $ ParserException err

tryParseLambda :: ASTOp m => String -> m (Maybe Text.Text, Maybe NodeRef)
tryParseLambda s = case words s of
    ("def" : name : _) -> do
        v <- IR.var "a"
        lam <- IR.generalize <$> IR.lam v v
        return (Just (Text.pack name), Just lam)
    ["->"] -> do
        v <- IR.var "a"
        lam <- IR.generalize <$> IR.lam v v
        return $ (Nothing, Just lam)
    ("->" : rest) -> do
        let (as, body) = partition ((== '$') . head) rest
        let args = fmap (drop 1) as
        argRefs <- mapM (IR.var' . stringToName) args
        (_, bodyRef) <- parseExpr $ unwords body
        lam <- lams argRefs bodyRef
        return $ (Nothing, Just lam)
    _ -> return (Nothing, Nothing)

data PortDefaultNotConstructibleException = PortDefaultNotConstructibleException PortDefault
    deriving Show

instance Exception PortDefaultNotConstructibleException where
    toException = astExceptionToException
    fromException = astExceptionFromException

parsePortDefault :: ASTOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)            = snd <$> parseExpr expr
parsePortDefault (Constant (IntValue i))      = IR.generalize <$> IR.number (fromIntegral i)
parsePortDefault (Constant (StringValue s))   = IR.generalize <$> IR.string s
parsePortDefault (Constant (DoubleValue d))   = $notImplemented
parsePortDefault (Constant (RationalValue r)) = $notImplemented
parsePortDefault (Constant (BoolValue b))     = IR.generalize <$> IR.cons_ (convert $ show b)
parsePortDefault d = throwM $ PortDefaultNotConstructibleException d
