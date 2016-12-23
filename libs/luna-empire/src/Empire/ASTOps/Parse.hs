{-# LANGUAGE OverloadedStrings #-}

module Empire.ASTOps.Parse where

import           Prologue

import           Control.Monad.Except         (throwError)
import           Data.List.Split              (splitOn)
import           Data.List                    (partition)

import           Empire.Data.AST              (NodeRef)
import           Empire.ASTOp                 (ASTOp)
import           Empire.ASTOps.Builder        (applyAccessors)
import           Empire.ASTOps.Remove         (safeRemove)

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))

import qualified Old.Luna.Syntax.Model.Network.Builder as Builder

import qualified Luna.Parser.Parser as Parser
import qualified Luna.Parser.State  as Parser
import qualified Luna.Parser.Term   as Term

parseExpr :: ASTOp m => String -> m (Maybe Text, NodeRef)
parseExpr s = do
    lamRes <- tryParseLambda s
    case lamRes of
        (name, Just l)  -> return (name, l)
        _               -> case parsed of
            Left d          -> throwError $ "Parser error: " <> show d
            Right (bldr, _) -> do
                r     <- bldr
                fixed <- applyAccessors r
                when (fixed /= r) $ safeRemove r
                return (Nothing, fixed)
          where
          operatorHotFix = replace "_.+" "_.op+"
                         . replace "_.*" "_.op*"
                         . replace "_./" "_.op/"
                         . replace "_.-" "_.op-"
                         . replace "_.^" "_.op^"
                         $ s
          parsed = Parser.parseString operatorHotFix $ Parser.parseGen Term.partial Parser.defState

tryParseLambda :: ASTOp m => String -> m (Maybe Text, Maybe NodeRef)
tryParseLambda s = case words s of
    ("def" : name : _) -> do
        v <- Builder.var $ fromString "in0"
        lam <- Builder.lam [Builder.arg v] v
        return (Just (convert name), Just lam)
    ["->"] -> do
        v <- Builder.var $ fromString "arg0"
        lam <- Builder.lam [Builder.arg v] v
        return $ (Nothing, Just lam)
    ("->" : rest) -> do
        let (as, body) = partition ((== '$') . head) rest
        let args = fmap (drop 1) as
        argRefs <- mapM (Builder.var . fromString) args
        (_, bodyRef) <- parseExpr $ unwords body
        let arg = Builder.arg <$> argRefs
        lam <- Builder.lam arg bodyRef
        return $ (Nothing, Just lam)
    _ -> return (Nothing, Nothing)

parsePortDefault :: ASTOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)          = snd <$> parseExpr expr
parsePortDefault (Constant (IntValue i))    = Builder.int $ fromIntegral i
parsePortDefault (Constant (StringValue s)) = Builder.str s
parsePortDefault (Constant (DoubleValue d)) = Builder.double d
parsePortDefault (Constant (BoolValue b))   = Builder.cons (fromString $ show b) []

replace :: String -> String -> String -> String
replace word with = intercalate with . splitOn word
