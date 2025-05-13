module Lexer
  ( lexemeOperator,
    LexemeOperator (..),
    lexemeIdent,
    lexemeKeyword,
    LexemeKeyword (..),
    lexemeNumber,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (when)
import Data.Char (isAlpha, isDigit, isSpace, toLower)
import Data.Functor (void)
import Data.List (elemIndex)
import Parser (Parser, char, failParser, mapChar, notFollowedBy, satisfy, string, try)

lexeme :: Parser v -> Parser v
lexeme p = p <* many (void (satisfy isSpace) <|> void lineComment <|> void multilineComment)
  where
    -- '//' (^'\n')*
    lineComment = (++) <$> try (string "//") <*> many (satisfy (/= '\n'))
    -- '/*' body
    multilineComment = (++) <$> string "/*" <*> body
      where
        -- '*/' | . body
        body = try (string "*/") <|> (:) <$> satisfy (const True) <*> body

data LexemeOperator
  = LLeftParen
  | LRightParen
  | LComma
  | LDot
  | LMinus
  | LPlus
  | LBitOr
  | LBitAnd
  | LBitNot
  | LLogicOr
  | LLogicAnd
  deriving (Eq)

lexemeOperator :: LexemeOperator -> Parser ()
lexemeOperator required = do
  -- note that, for example, && goes before &
  parsed <-
    lexeme
      ( op "(" LLeftParen
          <|> op ")" LRightParen
          <|> op "," LComma
          <|> op "." LDot
          <|> op "-" LMinus
          <|> op "+" LPlus
          <|> try (op "||" LLogicOr)
          <|> op "|" LBitOr
          <|> try (op "&&" LLogicAnd)
          <|> op "&" LBitAnd
          <|> op "~" LBitNot
      )
  when (parsed /= required) failParser
  where
    op s v = v <$ string s

identFirstCh :: Parser Char
identFirstCh = char '_' <|> satisfy isAlpha

identFollowingCh :: Parser Char
identFollowingCh = identFirstCh <|> satisfy isDigit

identOrKeyword :: Parser String
identOrKeyword = lexeme $ (:) <$> identFirstCh <*> many identFollowingCh

-- | Keyword is a special word that cannot be used as an identifier.
data LexemeKeyword = LIf | LExtendSign | LSwapHalves | LShiftLeft deriving (Eq)

keywordFromStr :: String -> Maybe LexemeKeyword
keywordFromStr "if" = Just LIf
keywordFromStr "signext" = Just LExtendSign
keywordFromStr "swaph" = Just LSwapHalves
keywordFromStr "shl" = Just LShiftLeft
keywordFromStr _ = Nothing

lexemeKeyword :: LexemeKeyword -> Parser ()
lexemeKeyword kw = do
  ik <- identOrKeyword
  maybe
    failParser
    (\v -> when (v /= kw) failParser)
    (keywordFromStr ik)

lexemeIdent :: Parser String
lexemeIdent = lexeme (try nonKw)
  where
    nonKw = do
      ik <- identOrKeyword
      maybe (return ik) (const failParser) (keywordFromStr ik)

data DigitBase = Bin | Oct | Dec | Hex

baseNum :: DigitBase -> Integer
baseNum Bin = 2
baseNum Oct = 8
baseNum Dec = 10
baseNum Hex = 16

digit :: DigitBase -> Parser Integer
digit base = toInteger <$> mapChar (\c -> toLower c `elemIndex` baseChars)
  where
    baseChars = case base of
      Bin -> "01"
      Oct -> "01234567"
      Dec -> "0123456789"
      Hex -> "0123456789abcdef"

lexemeNumber :: Parser Integer
lexemeNumber = lexeme (lit' <* notFollowedBy identFollowingCh)
  where
    lit' = (try (char '0') *> (pHex <|> pOct <|> pBin <|> num' Dec 0)) <|> num Dec

    pHex = char 'x' *> num Hex
    pOct = char 'o' *> num Oct
    pBin = char 'b' *> num Bin

    -- dig ('_'* dig)*
    num base = do
      d0 <- digit base
      num' base d0

    -- ('_'* dig)*
    num' base p =
      foldl (\a b -> baseNum base * a + b) p <$> many (many (char '_') *> digit base)
