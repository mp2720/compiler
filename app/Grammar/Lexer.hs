{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Grammar.Lexer (operator, ident, literal, keyword, sourcePos, Position (..), LexerState (..), Lexer, LexerResult) where

import Control.Applicative (Alternative (many, (<|>)), asum)
import Data.Char (isAlpha, isDigit, isSpace, toLower)
import Data.Function (on)
import Data.Functor (void)
import Data.List (elemIndex)
import Grammar.Combinators (Parser (..), ParserResult, State (..), failParser, mapTerm, notFollowedBy, satisfy, try)
import Control.Monad (unless)

data Position = Position
  { positionOffset :: Int,
    positionLine :: Int,
    positionColumn :: Int
  }
  deriving (Eq, Show)

data LexerState = LexerState
  { lexerInput :: String,
    lexerPosition :: Position,
    lexerTabSize :: Int
  }

instance State LexerState where
  type Term LexerState = Char
  didAdvance = on (/=) (positionOffset . lexerPosition)

  next s@(LexerState [] _ _) = (Nothing, s)
  next s@(LexerState (c : cs) pos tabLen) =
    ( Just c,
      s
        { lexerPosition = nextPosition,
          lexerInput = cs
        }
    )
    where
      nextPosition = Position nextOffset nextLine nextColumn
      nextOffset = positionOffset pos + 1
      nextLine = case c of
        '\n' -> line + 1
        _ -> line
      nextColumn = case c of
        '\r' -> column
        '\n' -> 0
        '\t' -> (column `div` tabLen + 1) * tabLen
        _ -> column + 1

      line = positionLine pos
      column = positionColumn pos

type Lexer = Parser LexerState

type LexerResult v = ParserResult LexerState v

char :: Char -> Lexer Char
char c = satisfy (== c)

string :: String -> Lexer String
string = traverse char

sourcePos :: Lexer Position
sourcePos = Parser $ \s -> Right (lexerPosition s, s)

lexeme :: Lexer a -> Lexer a
lexeme p = ignore *> p <* ignore

ignore :: Lexer ()
ignore = void $ many (void (satisfy isSpace) <|> void lineComment <|> void multilineComment)
  where
    -- '//' (^'\n')*
    lineComment = (++) <$> try (string "//") <*> many (satisfy (/= '\n'))
    -- '/*' rest
    multilineComment = (++) <$> try (string "/*") <*> rest
      where
        -- '*/' | . rest
        rest = try (string "*/") <|> (:) <$> satisfy (const True) <*> rest

operator :: String -> Lexer ()
operator expected =
  lexeme $
    asum
      ( map
          op
          [ "{",
            "}",
            "[",
            "]",
            "(",
            ")",
            ",",
            ".",
            "-",
            "+",
            "**",
            "*",
            "/",
            "%",
            "^",
            "||",
            "&&",
            "|",
            "&",
            "==",
            "=",
            "!=",
            "!",
            "<=",
            "<",
            ">=",
            ">",
            "~>=",
            "~>",
            "~<=",
            "~<",
            "~"
          ]
      )
  where
    op s = do
      p <- try $ string s
      unless (p == expected) failParser

identFirstCh :: Lexer Char
identFirstCh = char '_' <|> satisfy isAlpha

identFollowingCh :: Lexer Char
identFollowingCh = identFirstCh <|> satisfy isDigit

identOrKeyword :: Lexer String
identOrKeyword = (:) <$> identFirstCh <*> many identFollowingCh

ident :: Lexer String
ident = lexeme $ do
  i <- identOrKeyword
  if isKeyword i then failParser else return i

keyword :: String -> Lexer ()
keyword expected = lexeme $ do
  i <- identOrKeyword
  unless (isKeyword i && i == expected) failParser

isKeyword :: String -> Bool
isKeyword =
  ( `elem`
      [ "if",
        "while",
        "do",
        "return",
        "auto",
        "static",
        "volatile",
        "restrict",
        "const",
        "goto",
        "signext",
        "swaph",
        "shl",
        "shr",
        "asr"
      ]
  )

digit :: String -> Lexer Integer
digit digits = toInteger <$> mapTerm (\c -> toLower c `elemIndex` digits)

literal :: Lexer Integer
literal = lexeme $ lit' <* notFollowedBy identFollowingCh
  where
    lit' = (char '0' *> (hex <|> oct <|> bin <|> num' decDigits 10 0)) <|> num decDigits 10

    hex = char 'x' *> num "0123456789abcdef" 16
    oct = char 'o' *> num "01234567" 8
    bin = char 'b' *> num "01" 2

    decDigits = "0123456789"

    -- dig ('_'* dig)*
    num digits base = do
      d0 <- digit digits
      num' digits base d0

    -- ('_'* dig)*
    num' digits base p =
      foldl (\a b -> base * a + b) p <$> many (many (char '_') *> digit digits)
