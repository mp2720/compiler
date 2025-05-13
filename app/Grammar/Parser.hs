module Parser
  ( Position (..),
    State (..),
    ParserResult,
    Parser,
    try,
    lookAhead,
    notFollowedBy,
    mapChar,
    satisfy,
    char,
    string,
    mkState,
    sourcePos,
    failParser
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (liftM)
import Data.Function (on)
import GHC.Base (ap)

data Position = Position
  { positionOffset :: !Int,
    positionLine :: !Int,
    positionColumn :: !Int
  }
  deriving (Show)

instance Eq Position where
  -- It only makes sense to compare positions in the same input.
  (==) = (==) `on` positionOffset

data State = State
  { stateInput :: String,
    statePosition :: Position,
    stateTabSize :: Int
  }
  deriving (Show)

type ParserResult v = Either State (v, State)

-- | Simple parser combinator that works on String, tracks current position and just reports the first error
-- position. Use @try@ for backtracking.
newtype Parser v = Parser
  { runParser :: State -> ParserResult v
  }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure f = Parser $ \s -> Right (f, s)
  (<*>) = ap

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \s -> do
    (pv, s') <- p s
    runParser (f pv) s'

instance Alternative Parser where
  empty = Parser $ \s -> Left s
  (Parser p) <|> (Parser q) = Parser $ \s ->
    case p s of
      Left s'
        | ((==) `on` statePosition) s s' ->
            -- Position is the same, so no backtracking will happen unless @try@ is used.
            q s'
        | otherwise ->
            -- Fail to prevent backtracking.
            Left s'
      r -> r

try :: Parser v -> Parser v
try p = Parser $ \s -> case runParser p s of
  Left _ -> Left s
  right -> right

lookAhead :: Parser v -> Parser v
lookAhead p = Parser $ \s -> case runParser p s of
  Right (v, _) -> Right (v, s)
  left -> left

notFollowedBy :: Parser v -> Parser ()
notFollowedBy p = Parser $ \s -> case runParser p s of
  Left _ -> Right ((), s)
  Right _ -> Left s

nextCharState :: State -> State
nextCharState s@(State [] _ _) = s
nextCharState s@(State (c : cs) pos tabLen) =
  s
    { statePosition = nextPosition,
      stateInput = cs
    }
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

mapChar :: (Char -> Maybe a) -> Parser a
mapChar f = Parser $ \s ->
  case stateInput s of
    [] -> Left s
    x : _ -> case f x of
      Just a -> Right (a, nextCharState s)
      Nothing -> Left s

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = mapChar $ \c -> if predicate c then Just c else Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

mkState :: String -> State
mkState input = State input (Position 0 1 1) 8

sourcePos :: Parser Position
sourcePos = Parser $ \s -> Right (statePosition s, s)

failParser :: Parser a
failParser = Parser $ \s -> Left s
