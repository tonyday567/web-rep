{-# LANGUAGE OverloadedStrings #-}

-- | Parser helpers built on circuits-parser (replaces flatparse).
--
-- This module is exposed only for testing via doctest-parallel and is not intended to form part of the stable API.
module Web.Rep.Internal.FlatParse
  ( -- * Running
    runParserMaybe,
    runParserEither,
    runParser_,
    runParser,

    -- * Type
    Parser,

    -- * Parsers
    isWhitespace,
    ws_,
    ws,
    wss,
    nota,
    isa,
    sq,
    dq,
    wrappedDq,
    wrappedSq,
    wrappedQ,
    wrappedQNoGuard,
    eq,
    sep,
    bracketed,
    bracketedSB,
    wrapped,
    digit,
    digits,
    int,
    double,
    minus,
    signed,
    byteStringOf',
    comma,
    takeRest,

    -- * String/ByteString conversion
    strToUtf8,
    utf8ToStr,
  )
where

import Circuit.Parser
import Control.Monad (void)
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import GHC.Exts
import Prelude hiding (replicate)

-- | Run a Parser, throwing away leftovers. Returns Left on failure.
runParserEither :: Parser B.ByteString Char a -> ByteString -> Either ByteString a
runParserEither p b = case runParser p b of
  These a _ -> Right a
  This a -> Right a
  That _ -> Left "uncaught parse error"

-- | Run parser, discards leftovers & throws an error on failure.
runParser_ :: Parser B.ByteString Char a -> ByteString -> a
runParser_ p b = case runParser p b of
  These a t | B.null t -> a
  This a -> a
  _ -> error "uncaught parse error"

-- | Consume whitespace.
ws_ :: Parser B.ByteString Char ()
ws_ = skipWhile isWhitespace
{-# INLINE ws_ #-}

-- Whitespace predicate
isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\x0a' = True
isWhitespace '\x09' = True
isWhitespace '\x0c' = True
isWhitespace '\x0d' = True
isWhitespace _ = False
{-# INLINE isWhitespace #-}

-- | single whitespace
ws :: Parser B.ByteString Char Char
ws = satisfy isWhitespace

-- | multiple whitespace
wss :: Parser B.ByteString Char ByteString
wss = byteStringOf' $ some ws

-- | Single quote
sq :: Parser B.ByteString Char ()
sq = void (char '\'')

-- | Double quote
dq :: Parser B.ByteString Char ()
dq = void (char '"')

-- | Parse whilst not a specific character
nota :: Char -> Parser B.ByteString Char ByteString
nota c = byteStringOf' $ skipMany (satisfy (/= c))
{-# INLINE nota #-}

-- | Parse whilst satisfying a predicate.
isa :: (Char -> Bool) -> Parser B.ByteString Char ByteString
isa p = byteStringOf' $ skipMany (satisfy p)
{-# INLINE isa #-}

-- | Capture consumed input.
byteStringOf' :: Parser B.ByteString Char a -> Parser B.ByteString Char ByteString
byteStringOf' p = fst <$> captured p
{-# INLINE byteStringOf' #-}

-- | A single-quoted string.
wrappedSq :: Parser B.ByteString Char ByteString
wrappedSq = sq *> nota '\'' <* sq
{-# INLINE wrappedSq #-}

-- | A double-quoted string.
wrappedDq :: Parser B.ByteString Char ByteString
wrappedDq = dq *> nota '"' <* dq
{-# INLINE wrappedDq #-}

-- | A single-quoted or double-quoted string.
wrappedQ :: Parser B.ByteString Char ByteString
wrappedQ = wrappedDq <|> wrappedSq
{-# INLINE wrappedQ #-}

-- | A single-quoted or double-quoted wrapped parser.
wrappedQNoGuard :: Parser B.ByteString Char a -> Parser B.ByteString Char a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

-- | eq production: = with optional whitespace around
eq :: Parser B.ByteString Char ()
eq = ws_ *> void (char '=') <* ws_
{-# INLINE eq #-}

-- | Some with a separator.
sep :: Parser B.ByteString Char s -> Parser B.ByteString Char a -> Parser B.ByteString Char [a]
sep s p = (:) <$> p <*> many (s *> p)

-- | Parser bracketed by two other parsers.
bracketed :: Parser B.ByteString Char b -> Parser B.ByteString Char b -> Parser B.ByteString Char a -> Parser B.ByteString Char a
bracketed o c p = o *> p <* c
{-# INLINE bracketed #-}

-- | Parser bracketed by square brackets.
bracketedSB :: Parser B.ByteString Char String
bracketedSB = bracketed (void (char '[')) (void (char ']')) (many (satisfy (/= ']')))

-- | Parser wrapped by another parser.
wrapped :: Parser B.ByteString Char () -> Parser B.ByteString Char a -> Parser B.ByteString Char a
wrapped x p = bracketed x x p
{-# INLINE wrapped #-}

-- | A single digit
digit :: Parser B.ByteString Char Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | An (unsigned) 'Int' parser
int :: Parser B.ByteString Char Int
int = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser B.ByteString Char (Int, Int)
digits = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure (place, n)

-- | A 'Double' parser.
double :: Parser B.ByteString Char Double
double = do
  (placel, nl) <- digits
  withOption
    (void (char '.') *> digits)
    ( \(placer, nr) ->
        case placel of
          1 -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

minus :: Parser B.ByteString Char ()
minus = void (char '-') <|> void (string "¯")

stringBs bs = void (string (B.unpack bs))

-- | Parser for a signed prefix to a number.
signed :: (Num b) => Parser B.ByteString Char b -> Parser B.ByteString Char b
signed p = do
  m <- optional minus
  case m of
    Nothing -> p
    Just () -> negate <$> p

-- | Comma parser
comma :: Parser B.ByteString Char ()
comma = void (char ',')

-- | Convert a 'String' to a UTF-8 encoded 'ByteString'.
strToUtf8 :: String -> ByteString
strToUtf8 = encodeUtf8 . T.pack

-- | Convert a UTF-8 encoded 'ByteString' to a 'String'.
utf8ToStr :: ByteString -> String
utf8ToStr = T.unpack . decodeUtf8With lenientDecode
