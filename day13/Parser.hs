-- | Simple applicative parser combinators (mostly compatible with Parsec).
--
-- Example (parsing pairs of non-negative integers):
--
-- > (,) <$ char '(' <*> nat <* char ',' <*> nat <* char ')'
--
-- These parsers backtrack extensively, so use them on short strings.
module Parser (
    Parser,
    parseAll,
    parsePrefix,
    -- * Combinations
    (<|>),
    -- * Lists
    many, some, sepBy1,
    -- * Numbers
    nat, int,
    -- * Literal matches
    string, char,
    -- * Single characters
    satisfy, space, digit, letter, anyChar,
    ) where

import Control.Applicative
import Data.Functor
import Data.Char
import Data.List

-- | Simple applicative parser
data Parser a = P (String -> [(a, String)])

-- | Run a parser on a string, returning all possible parses of prefixes
-- of the string, each paired with the unparsed rest of the string.
parsePrefix :: Parser a -> String -> [(a, String)]
parsePrefix (P p) = p

instance Functor Parser where
    fmap f p = P $ \ s -> [(f x, t) | (x, t) <- parsePrefix p s]

instance Applicative Parser where
    pure x = P $ \ s -> [(x, s)]
    pf <*> px =
        P $ \ s ->
            [(f x, u) | (f, t) <- parsePrefix pf s, (x, u) <- parsePrefix px t]

instance Alternative Parser where
    empty = P (const [])
    px <|> py = P $ \ s -> parsePrefix px s ++ parsePrefix py s

-- | Run a parser on a string, returning all possible parses of the
-- full string.
parseAll :: Parser a -> String -> [a]
parseAll p s = [x | (x, t) <- parsePrefix p s, null t]

-- specialized parsers

-- | @'satisfy' p@ matches any single character on which @p@ returns 'True'.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P satisfyP
  where
    satisfyP (c:s) | p c = [(c, s)]
    satisfyP _ = []

-- | Literal match of a given character.
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Matches a space character.
space :: Parser Char
space = satisfy isSpace

-- | Matches any digit.
digit :: Parser Char
digit = satisfy isDigit

-- | Matches any letter.
letter :: Parser Char
letter = satisfy isAlpha

-- | This parser succeeds for any character, and returns the parsed character.
anyChar :: Parser Char
anyChar = satisfy (const True)

-- | Parse a non-negative integer (a natural number).
nat :: Parser Int
nat = read <$> some digit

-- | Parse an integer.
int :: Parser Int
int = negate <$ char '-' <*> nat <|> nat

-- | Literal match of the given string.
string :: String -> Parser String
string str = P matchStr
  where
    matchStr t
      | take n t == str = [(str, drop (length str) t)]
      | otherwise = []
    n = length str

-- general combinators

-- | @'sepBy1' p sep@ parses one or more occurrences of @p@, separated
-- by @sep@, and returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
