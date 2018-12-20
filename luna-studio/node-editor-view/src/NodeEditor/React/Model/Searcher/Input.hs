{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Input where

import Common.Prelude

import qualified Data.Text              as Text
import qualified Data.Vector.Unboxed    as Vector
import qualified Luna.Syntax.Text.Lexer as Lexer

import Data.Convert (Convertible (convert))
import Data.Text32  (Text32)



----------------------
-- === Divided === ---
----------------------


-- === Definition === ---

data Divided = Divided
    { _prefix :: Text
    , _query  :: Text
    , _suffix :: Text
    } deriving (Eq, Generic, Show)

makeLenses ''Divided

instance Default     Divided where def = Divided mempty mempty mempty
instance NFData      Divided
instance Convertible Divided Text where
    convert d = let
        p = d ^. prefix
        q = d ^. query
        s = d ^. suffix
        in p <> q <> s



--------------------
-- === Input === ---
--------------------


-- === Definition === ---

data Input
    = RawInput     Text
    | DividedInput Divided
    deriving (Eq, Generic, Show)

makePrisms ''Input

instance Default     Input where def = RawInput def
instance NFData      Input
instance Convertible Input Text where
    convert (RawInput     t) = t
    convert (DividedInput d) = convert d


-- === API === --

fromStream :: Text -> [Lexer.Token Lexer.Symbol] -> Int -> Input
fromStream input inputStream cursorPos = let
    mayQueryBegin = recursiveFindQueryBegin inputStream cursorPos
    splitInput    = \qBeg -> let
        (pref', suff') = Text.splitAt cursorPos input
        (pref , q')    = Text.splitAt qBeg pref'
        (q'', suff)    = Text.breakOn " " suff'
        q              = q' <> q''
        in DividedInput $! Divided pref q suff
    in case mayQueryBegin of
        Nothing  -> RawInput input
        Just beg -> splitInput beg
{-# INLINE fromStream #-}

findLambdaArgsAndEndOfLambdaArgs :: Text32 -> [Lexer.Token Lexer.Symbol]
    -> Maybe ([Text], Int)
findLambdaArgsAndEndOfLambdaArgs input' tokens = result where
    result         = findRecursive tokens (0 :: Int) 0 def def
    exprLength   t = fromIntegral $! t ^. Lexer.span
    offsetLength t = fromIntegral $! t ^. Lexer.offset
    getArg   beg t = Vector.slice beg (exprLength t) input'
    tokenLength  t = exprLength t + offsetLength t
    findRecursive []    _                     _      _    res = res
    findRecursive (h:t) openParanthesisNumber endPos args res = let
        findR paranthesisModifier arguments res' = findRecursive
            t
            (paranthesisModifier openParanthesisNumber)
            (endPos + tokenLength h)
            arguments
            res'
        updateResult     = Just (fmap convert $! args, endPos + exprLength h)
        blockStartResult = if openParanthesisNumber == 0
            then updateResult
            else res
        varArgs = getArg endPos h : args
        in case h ^. Lexer.element of
            Lexer.BlockStart             -> findR id   args    blockStartResult
            Lexer.Var        {}          -> findR id   varArgs res
            Lexer.Block      Lexer.Begin -> findR succ args    res
            Lexer.Block      Lexer.End   -> findR pred args    res
            _                            -> findR id   args    res


-- === Utils === --

isQuerySymbol :: Lexer.Symbol -> Bool
isQuerySymbol (Lexer.Var      {}) = True
isQuerySymbol (Lexer.Cons     {}) = True
isQuerySymbol (Lexer.Wildcard {}) = True
isQuerySymbol (Lexer.KwAll    {}) = True
isQuerySymbol (Lexer.KwCase   {}) = True
isQuerySymbol (Lexer.KwClass  {}) = True
isQuerySymbol (Lexer.KwDef    {}) = True
isQuerySymbol (Lexer.KwImport {}) = True
isQuerySymbol (Lexer.KwOf     {}) = True
isQuerySymbol (Lexer.Operator {}) = True
isQuerySymbol (Lexer.Modifier {}) = True
isQuerySymbol _                   = False
{-# INLINE isQuerySymbol #-}

isInString :: Lexer.Symbol -> Bool
isInString (Lexer.Str    {})           = True
isInString (Lexer.StrEsc {})           = True
isInString (Lexer.Quote _ Lexer.Begin) = True
isInString _                           = False
{-# INLINE isInString #-}

recursiveFindQueryBegin :: [Lexer.Token Lexer.Symbol] -> Int -> Maybe Int
recursiveFindQueryBegin []    _         = Nothing
recursiveFindQueryBegin (h:t) cursorPos = let
    hSpan             = h ^. Lexer.span
    hSpanInt          = fromIntegral hSpan
    hOffset           = h ^. Lexer.offset
    hElement          = h ^. Lexer.element
    tokenLength       = fromIntegral $! hSpan + hOffset
    cursorPosInSuffix = cursorPos - tokenLength
    appendTokenLength = \r -> tokenLength + r
    maySuffixResult   = recursiveFindQueryBegin t cursorPosInSuffix
    skipWord          = appendTokenLength <$> maySuffixResult
    notInString       = not $! isInString hElement
    isHQuerySymbol    = isQuerySymbol hElement
    in if cursorPos > tokenLength                       then skipWord
        else if cursorPos <= hSpanInt && isHQuerySymbol then Just 0
        else if cursorPos >  hSpanInt && notInString    then Just cursorPos
        else Nothing
