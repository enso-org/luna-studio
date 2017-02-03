{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.ASTOps.Parse (
    parseExpr
  , parsePortDefault
  ) where

import           Empire.Prelude

import           Data.Char                    (isLetter)
import           Data.List.Split              (splitOn)
import           Data.List                    (partition, takeWhile)
import           Data.Ratio                   (approxRational)
import qualified Data.Text                    as Text
import           Text.Read                    (readMaybe)

import           Empire.Data.AST              (NodeRef, astExceptionToException,
                                               astExceptionFromException)
import           Empire.ASTOps.Builder        (buildAccessors, lams)
import           Empire.ASTOp                 (ASTOp, EmpirePass)

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))

import qualified Luna.IR as IR
import qualified Luna.Pass as Pass
import qualified Luna.Passes.Transform.Parsing.Parsing as Parsing
import qualified Luna.Passes.Transform.Parsing.OffsetParser as OffsetParser
import qualified Luna.Passes.Transform.Parsing.Parser as Parser
import qualified Data.SpanTree as SpanTree
import qualified Text.Megaparsec as Megaparsec

data ParserException e = ParserException e
    deriving (Show)

instance Exception e => Exception (ParserException e) where
    toException = astExceptionToException
    fromException = astExceptionFromException
    displayException (ParserException e) = "ParserException (" ++ displayException e ++ ")"

parseExpr :: ASTOp m => String -> m (Maybe Text.Text, NodeRef)
parseExpr s = do
  lamRes <- tryParseLambda s
  parsed <- liftIO $ Parsing.testParse (OffsetParser.evalOffsetParserT $ SpanTree.evalSpanBuilderT (Parsing.testMe <* Megaparsec.eof)) s
  case lamRes of
      (name, Just l)  -> return (name, l)
      _               -> case parsed of
          Right (Parser.IRBuilder x) -> do
              x' <- x
              return (Nothing, x')
          Left err -> throwM $ ParserException err

tryParseAccessors :: ASTOp m => String -> m (Maybe NodeRef)
tryParseAccessors s = case splitOn "." s of
    []  -> return Nothing
    [_a] -> return Nothing
    (var:accs) -> do
        v <- IR.generalize <$> IR.strVar var
        node <- buildAccessors v accs
        return $ Just node

tryParseLambda :: ASTOp m => String -> m (Maybe Text.Text, Maybe NodeRef)
tryParseLambda s = case words s of
    ("def" : name : _) -> do
        v <- IR.strVar "in0"
        lam <- IR.generalize <$> IR.lam v v
        return (Just (Text.pack name), Just lam)
    ["->"] -> do
        v <- IR.strVar "arg0"
        lam <- IR.generalize <$> IR.lam v v
        return $ (Nothing, Just lam)
    ("->" : rest) -> do
        let (as, body) = partition ((== '$') . head) rest
        let args = fmap (drop 1) as
        argRefs <- mapM IR.strVar args
        (_, bodyRef) <- parseExpr $ unwords body
        lam <- lams (map IR.generalize argRefs) bodyRef
        return $ (Nothing, Just lam)
    _ -> return (Nothing, Nothing)

data PortDefaultNotConstructibleException = PortDefaultNotConstructibleException PortDefault
    deriving Show

instance Exception PortDefaultNotConstructibleException where
    toException = astExceptionToException
    fromException = astExceptionFromException

parsePortDefault :: ASTOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)          = snd <$> parseExpr expr
parsePortDefault (Constant (IntValue i))    = IR.generalize <$> IR.number (fromIntegral i)
parsePortDefault (Constant (StringValue s)) = IR.generalize <$> IR.string s
parsePortDefault (Constant (DoubleValue d)) = IR.generalize <$> IR.number (approxRational d 0.1)
parsePortDefault (Constant (RationalValue r)) = IR.generalize <$> IR.number r
parsePortDefault (Constant (BoolValue b))   = do
    bool' <- IR.string $ show b
    IR.generalize <$> IR.cons_ bool'
parsePortDefault d = throwM $ PortDefaultNotConstructibleException d
