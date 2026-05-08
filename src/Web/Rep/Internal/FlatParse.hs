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
  )
where

import Circuit.Parser
import Data.Bool
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text qualified as T
import Data.ByteString.Char8 qualified as B
import Data.Char hiding (isDigit)
import GHC.Exts
import Prelude hiding (replicate)

-- | Run a Parser, throwing away leftovers. Nothing on failure.
runParserMaybe :: Parser Text Char a -> ByteString -> Maybe a
runParserMaybe p b = case runParser p b of
  These r _ -> Just r
  This r    -> Just r
  That _    -> Nothing

-- | Run a Parser, throwing away leftovers. Returns Left on failure.
runParserEither :: Parser Text Char a -> ByteString -> Either ByteString a
runParserEither p b = case runParser p b of
  These a _ -> Right a
  This a    -> Right a
  That _    -> Left "uncaught parse error"

-- | Run parser, discards leftovers & throws an error on failure.
runParser_ :: Parser Text Char a -> ByteString -> a
runParser_ p b = case runParser p b of
  These a "" -> a
  This a     -> a
  _          -> error "uncaught parse error"

-- | Consume whitespace.
ws_ :: Parser Text Char ()
ws_ = skipWhile isWhitespace
{-# INLINE ws_ #-}

-- Whitespace predicate
isWhitespace :: Char -> Bool
isWhitespace ' '     = True
isWhitespace '\x0a'  = True
isWhitespace '\x09'  = True
isWhitespace '\x0c'  = True
isWhitespace '\x0d'  = True
isWhitespace _       = False
{-# INLINE isWhitespace #-}

-- | single whitespace
ws :: Parser Text Char Char
ws = satisfy isWhitespace

-- | multiple whitespace
wss :: Parser Text Char ByteString
wss = byteStringOf' $ some ws

-- | Single quote
sq :: Parser Text Char ()
sq = () <$ char '\''

-- | Double quote
dq :: Parser Text Char ()
dq = () <$ char '"'

-- | Parse whilst not a specific character
nota :: Char -> Parser Text Char ByteString
nota c = byteStringOf' $ skipMany (satisfy (/= c))
{-# INLINE nota #-}

-- | Parse whilst satisfying a predicate.
isa :: (Char -> Bool) -> Parser Text Char ByteString
isa p = byteStringOf' $ skipMany (satisfy p)
{-# INLINE isa #-}

-- | Capture consumed input.
byteStringOf' :: Parser Text Char a -> Parser Text Char ByteString
byteStringOf' p = fst <$> captured p
{-# INLINE byteStringOf' #-}

-- | A single-quoted string.
wrappedSq :: Parser Text Char ByteString
wrappedSq = sq *> nota '\'' <* sq
{-# INLINE wrappedSq #-}

-- | A double-quoted string.
wrappedDq :: Parser Text Char ByteString
wrappedDq = dq *> nota '"' <* dq
{-# INLINE wrappedDq #-}

-- | A single-quoted or double-quoted string.
wrappedQ :: Parser Text Char ByteString
wrappedQ = wrappedDq <|> wrappedSq
{-# INLINE wrappedQ #-}

-- | A single-quoted or double-quoted wrapped parser.
wrappedQNoGuard :: Parser Text Char a -> Parser Text Char a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

-- | eq production: = with optional whitespace around
eq :: Parser Text Char ()
eq = ws_ *> (() <$ char '=') <* ws_
{-# INLINE eq #-}

-- | Some with a separator.
sep :: Parser Text Char s -> Parser Text Char a -> Parser Text Char [a]
sep s p = (:) <$> p <*> many (s *> p)

-- | Parser bracketed by two other parsers.
bracketed :: Parser Text Char b -> Parser Text Char b -> Parser Text Char a -> Parser Text Char a
bracketed o c p = o *> p <* c
{-# INLINE bracketed #-}

-- | Parser bracketed by square brackets.
bracketedSB :: Parser Text Char String
bracketedSB = bracketed (() <$ char '[') (() <$ char ']') (many (satisfy (/= ']')))

-- | Parser wrapped by another parser.
wrapped :: Parser Text Char () -> Parser Text Char a -> Parser Text Char a
wrapped x p = bracketed x x p
{-# INLINE wrapped #-}

-- | A single digit
digit :: Parser Text Char Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | An (unsigned) 'Int' parser
int :: Parser Text Char Int
int = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser Text Char (Int, Int)
digits = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure (place, n)

-- | A 'Double' parser.
double :: Parser Text Char Double
double = do
  (placel, nl) <- digits
  withOption
    ((() <$ char '.') *> digits)
    ( \(placer, nr) ->
        case placel of
          1 -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

minus :: Parser Text Char ()
minus = (() <$ char '-') <|> string "¯"

stringBs bs = () <$ string (decodeUtf8With lenientDecode bs)

-- | Parser for a signed prefix to a number.
signed :: (Num b) => Parser Text Char b -> Parser Text Char b
signed p = do
  m <- optional minus
  case m of
    Nothing -> p
    Just () -> negate <$> p

-- | Comma parser
comma :: Parser Text Char ()
comma = () <$ char ','
