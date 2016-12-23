{-# LANGUAGE OverloadedStrings #-}

module Empire.ASTOps.Parse where

import           Empire.Prelude

import           Data.Char                    (isLetter)
import           Data.List.Split              (splitOn)
import           Data.List                    (intercalate, partition, takeWhile)
import           Data.Ratio                   (approxRational)
import qualified Data.Text.Lazy               as Text
import           Text.Read                    (readMaybe)

import           Empire.Data.AST              (NodeRef, astExceptionToException,
                                               astExceptionFromException)
import           Empire.ASTOp                 (ASTOp, lams)

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))

import qualified Luna.IR.Function as IR (arg)
import qualified Luna.IR as IR

parseExpr :: ASTOp m => String -> m (Maybe Text.Text, NodeRef)
parseExpr s = do
  lamRes <- tryParseLambda s
  case lamRes of
      (name, Just l)  -> return (name, l)
      _               -> case (readMaybe s :: Maybe Int) of
          Just i -> do
              i' <- IR.generalize <$> IR.integer i
              return (Nothing, i')
          _      -> case takeWhile isLetter s of
              [] -> $notImplemented
              v -> IR.strVar v >>= \v' -> return (Nothing, IR.generalize v')

tryParseLambda :: ASTOp m => String -> m (Maybe Text.Text, Maybe NodeRef)
tryParseLambda s = case words s of
    ("def" : name : _) -> do
        v <- IR.strVar "in0"
        lam <- IR.generalize <$> IR.lam (IR.arg v) v
        return (Just (Text.pack name), Just lam)
    ["->"] -> do
        v <- IR.strVar "arg0"
        lam <- IR.generalize <$> IR.lam (IR.arg v) v
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
parsePortDefault (Constant (IntValue i))    = IR.generalize <$> IR.integer i
parsePortDefault (Constant (StringValue s)) = IR.generalize <$> IR.string s
parsePortDefault (Constant (DoubleValue d)) = IR.generalize <$> IR.rational (approxRational d 0.1)
parsePortDefault (Constant (RationalValue r)) = IR.generalize <$> IR.rational r
parsePortDefault (Constant (BoolValue b))   = do
    bool' <- IR.string $ show b
    IR.generalize <$> IR.cons bool'
parsePortDefault d = throwM $ PortDefaultNotConstructibleException d

replace :: String -> String -> String -> String
replace word with = intercalate with . splitOn word
