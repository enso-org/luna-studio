{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Input where

import Common.Prelude

import qualified Data.Text              as Text
import qualified Data.Vector.Unboxed    as Vector
import qualified Luna.Syntax.Text.Lexer as Lexer

import Data.Convert (Convertible (convert))
import Data.Text32  (Text32)


-------------------------
-- === SymbolKind === ---
-------------------------

-- === Definition === ---

data SymbolKind
    = ExpressionStart
    | Function
    | Argument
    | Method
    | Operator
    deriving (Eq, Show, Generic)
makePrisms ''SymbolKind

instance Default SymbolKind where def = ExpressionStart
instance NFData  SymbolKind

----------------------
-- === Divided === ---
----------------------

-- === Definition === ---

-- | This datatype is a result of analysing the tokenized searcher input,
--   specifying relevant parts of the query and the prediction for next
--   symbol that should be typed in.
--   The query field is the word that is currently being searched, while
--   prefix and suffix are the remaining bits, not relevant for current search.
--   Full searcher input can be rebuilt as prefix <> query <> suffix.
data Divided = Divided
    { _prefix               :: Text
    , _query                :: Text
    , _suffix               :: Text
    , _nextSymbolPrediction :: SymbolKind
    } deriving (Eq, Generic, Show)

makeLenses ''Divided

instance Default     Divided where def = Divided mempty mempty mempty def
instance NFData      Divided
instance Convertible Divided Text where
    convert d = let
        p = d ^. prefix
        q = d ^. query
        s = d ^. suffix
        in p <> q <> s

null :: Divided -> Bool
null = Text.null . convert

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
    splitInput qBeg = let
        (beforeCursor, afterCursor) = Text.splitAt cursorPos input
        (pref, queryBeforeCursor)   = Text.splitAt qBeg beforeCursor
        (queryAfterCursor, suff)    = Text.breakOn " " afterCursor
        query      = queryBeforeCursor <> queryAfterCursor
        nextSymbol = predictSymbolKind inputStream cursorPos
        in DividedInput $ Divided pref query suff nextSymbol
    in case mayQueryBegin of
        Nothing  -> RawInput input
        Just beg -> splitInput beg
{-# INLINE fromStream #-}

findLambdaArgsAndEndOfLambdaArgs :: Text32 -> [Lexer.Token Lexer.Symbol]
    -> Maybe ([Text], Int)
findLambdaArgsAndEndOfLambdaArgs input' tokens = result where
    result         = findRecursive tokens (0 :: Int) 0 def def
    exprLength   t = fromIntegral $ t ^. Lexer.span
    offsetLength t = fromIntegral $ t ^. Lexer.offset
    getArg   beg t = Vector.slice beg (exprLength t) input'
    tokenLength  t = exprLength t + offsetLength t
    findRecursive []    _                     _      _    res = res
    findRecursive (tok:toks) openParanthesisNumber endPos args res = let
        findR paranthesisModifier = findRecursive
            toks
            (paranthesisModifier openParanthesisNumber)
            (endPos + tokenLength tok)
        updateResult     = Just (convert <$> args, endPos + exprLength tok)
        blockStartResult = if openParanthesisNumber == 0
            then updateResult
            else res
        varArgs = getArg endPos tok : args
        in case tok ^. Lexer.element of
            Lexer.BlockStart             -> findR id   args    blockStartResult
            Lexer.Var        {}          -> findR id   varArgs res
            Lexer.Block      Lexer.Begin -> findR succ args    res
            Lexer.Block      Lexer.End   -> findR pred args    res
            _                            -> findR id   args    res


-- === Utils === --

isQuerySymbol :: Lexer.Symbol -> Bool
isQuerySymbol = \case
    Lexer.Var      {}  -> True
    Lexer.Cons     {}  -> True
    Lexer.Wildcard {}  -> True
    Lexer.KwAll    {}  -> True
    Lexer.KwCase   {}  -> True
    Lexer.KwClass  {}  -> True
    Lexer.KwDef    {}  -> True
    Lexer.KwImport {}  -> True
    Lexer.KwOf     {}  -> True
    (Lexer.Operator o) -> o /= ","
    Lexer.Modifier {}  -> True
    _                  -> False
{-# INLINE isQuerySymbol #-}

isInString :: Lexer.Symbol -> Bool
isInString = \case
    Lexer.Str    {}             -> True
    Lexer.StrEsc {}             -> True
    (Lexer.Quote _ Lexer.Begin) -> True
    _                           -> False
{-# INLINE isInString #-}

breaksQuery :: Lexer.Symbol -> Bool
breaksQuery = \case
    Lexer.Accessor   {}       -> True
    Lexer.Group      {}       -> True
    Lexer.List       {}       -> True
    Lexer.BlockStart {}       -> True
    (Lexer.Quote _ Lexer.End) -> True
    (Lexer.Operator ",")      -> True
    _                         -> False

recursiveFindQueryBegin :: [Lexer.Token Lexer.Symbol] -> Int -> Maybe Int
recursiveFindQueryBegin []    _         = Nothing
recursiveFindQueryBegin (h:t) cursorPos = let
    hSpan              = h ^. Lexer.span
    hSpanInt           = fromIntegral hSpan
    hOffset            = h ^. Lexer.offset
    hElement           = h ^. Lexer.element
    tokenLength        = fromIntegral $ hSpan + hOffset
    cursorPosInSuffix  = cursorPos - tokenLength
    appendTokenLength  = (tokenLength +)
    maySuffixResult    = recursiveFindQueryBegin t cursorPosInSuffix
    skipWord           = appendTokenLength <$> maySuffixResult
    notInString        = not $ isInString hElement
    queryBreak         = breaksQuery hElement
    isHQuerySymbol     = isQuerySymbol hElement
    cursorIsAfterToken = cursorPos > tokenLength
    cursorIsInsideTokenSpan = cursorPos <= hSpanInt && isHQuerySymbol
    cursorIsAfterTokenSpan = cursorPos > hSpanInt
    cursorIsAtQueryEndAndStartsNewToken = cursorPos >= hSpanInt && queryBreak
    cursorStartsNewToken = (cursorIsAfterTokenSpan && notInString)
                         || cursorIsAtQueryEndAndStartsNewToken
    in if cursorIsAfterToken            then skipWord
        else if cursorIsInsideTokenSpan then Just 0
        else if cursorStartsNewToken    then Just cursorPos
        else Nothing

predictSymbolKind :: [Lexer.Token Lexer.Symbol] -> Int -> SymbolKind
predictSymbolKind = go ExpressionStart where
    go kind []             _   = kind
    go kind (token:tokens) pos = let
        tokSpan   = fromIntegral $ token ^. Lexer.span
        tokOffset = fromIntegral $ token ^. Lexer.offset
        tokLen    = tokSpan + tokOffset
        symbol    = token ^. Lexer.element
        cursorIsInsideToken     = tokSpan > pos
        cursorIsAtTheEndOfToken = tokSpan == pos
        queryContinues          = not (breaksQuery symbol)
        in if cursorIsInsideToken || (cursorIsAtTheEndOfToken && queryContinues)
            then kind
            else go (nextSymbolKind symbol kind) tokens (pos - tokLen)

    nextSymbolKind = curry $ \case
        (Lexer.Var        {},     _)               -> Argument
        (Lexer.Operator   {},     _)               -> Argument
        (Lexer.Cons       {},     _)               -> Argument
        (Lexer.Accessor   {},     _)               -> Method
        (Lexer.BlockStart {},     _)               -> Function
        (Lexer.Group Lexer.Begin, _)               -> Function
        (Lexer.Group Lexer.End,   _)               -> Argument
        (Lexer.List  Lexer.Begin, _)               -> Argument
        (Lexer.Number {},         Function)        -> Operator
        (Lexer.Number {},         ExpressionStart) -> Operator
        (Lexer.Quote _ Lexer.End, Function)        -> Operator
        (Lexer.Quote _ Lexer.End, ExpressionStart) -> Operator
        (_,                       k)               -> k

