{-# LANGUAGE OverloadedStrings #-}

module Empire.ASTOps.Parse where

import           Prologue

import           Control.Monad.Except         (throwError)
import           Data.List.Split              (splitOn)
import           Data.List                    (partition)
import           Data.Ratio                   (approxRational)
import qualified Data.Text.Lazy               as Text

import           Empire.Data.AST              (NodeRef)
import           Empire.ASTOp                 (ASTOp)
import           Empire.ASTOps.Builder        (applyAccessors)
import           Empire.ASTOps.Remove         (safeRemove)

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))

import qualified Luna.IR as IR

parseExpr :: ASTOp m => String -> m (Maybe Text, NodeRef)
parseExpr s = do
    b <- IR.generalize <$> IR.blank
    return (Nothing, b)

tryParseLambda :: ASTOp m => String -> m (Maybe Text, Maybe NodeRef)
tryParseLambda s = return (Nothing, Nothing)

parsePortDefault :: ASTOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)          = snd <$> parseExpr expr
parsePortDefault (Constant (IntValue i))    = IR.generalize <$> IR.integer i
parsePortDefault (Constant (StringValue s)) = IR.generalize <$> IR.rawString s
parsePortDefault (Constant (DoubleValue d)) = IR.generalize <$> IR.rational (approxRational d 0.1)
parsePortDefault (Constant (BoolValue b))   = do
    bool' <- IR.rawString $ show b
    IR.generalize <$> IR.cons bool'

replace :: String -> String -> String -> String
replace word with = intercalate with . splitOn word
