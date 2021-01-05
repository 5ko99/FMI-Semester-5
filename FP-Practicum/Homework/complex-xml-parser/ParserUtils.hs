{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- -# LANGUAGE TupleSections #-}

module ParserUtils where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace, ord)
import Prelude hiding (span)
import qualified Prelude

data ParserError = ParserError
  { msg :: String,
    pos :: Int
  }
  deriving (Show)

abort :: String -> Int -> Either ParserError b
abort msg = Left . ParserError msg

data ParserSuccess a = ParserSuccess
  { rest :: String,
    consumedTokens :: Int,
    result :: a
  }
  deriving (Show)

type ParserResult a = Either ParserError (ParserSuccess a)

newtype Parser a = Parser
  { runParser :: String -> ParserResult a
  }

abortParser :: String -> Int -> Parser a
abortParser error pos = Parser $ \_ -> abort error pos

char :: Char -> Parser Char
char c = cond (== c)

cond :: (Char -> Bool) -> Parser Char
cond c = Parser $
  \case
    (h : t) | c h -> Right (ParserSuccess {rest = t, consumedTokens = 1, result = h})
    _ -> abort "could not parse with condition" 0

nom :: Parser Char
nom = cond (const True)

nomAll :: Parser String
nomAll = many nom

instance Functor Parser where
  fmap mapper parser =
    Parser $
      fmap
        ( \ParserSuccess {..} ->
            ParserSuccess
              { rest = rest,
                consumedTokens = consumedTokens,
                result = mapper result
              }
        )
        . runParser parser

instance Applicative Parser where
  pure a = Parser $ Right . (\input -> ParserSuccess {rest = input, consumedTokens = 0, result = a})
  Parser fp <*> Parser vp = Parser $ \i -> do
    ParserSuccess {rest = rest', consumedTokens = ct', result = result'} <- fp i
    ParserSuccess {rest = rest'', consumedTokens = ct'', result = result''} <- vp rest'
    return $ ParserSuccess {rest = rest'', consumedTokens = ct'' + ct', result = result' result''}

instance Monad Parser where
  Parser p >>= func = Parser $ \i -> do
    ParserSuccess {..} <- p i
    runParser (func result) rest

instance Alternative Parser where
  empty = Parser $ \_ -> abort "" 0
  Parser pa <|> Parser pb = Parser $ \i ->
    case pa i of
      Left _ -> pb i
      result -> result

tryRead :: Read a => String -> Parser a
tryRead string = do
  case reads string of
    [(result, _)] -> return result
    _ -> abortParser "could not read value" 0

bool :: Parser Bool
bool =
  (string "True" <|> string "False") >>= tryRead

string :: String -> Parser String
string = mapM char

{--
span :: (Char -> Bool) -> Parser String
span predicate = Parser $ \i ->
  let (matched, rest) = Prelude.span predicate i in Right (rest, matched)

alphaNum :: Parser String
alphaNum = span isAlphaNum

stringLiteral :: Parser String
stringLiteral = char '"' *> span (/= '"') <* char '"'

--My code
tagHelper c = (c /= '=') && (c /= '<') && (c /= '>')

tag :: Parser String
tag = span tagHelper

closingName :: Parser String
closingName = span (/= '>')

noEndOrArrow :: Char -> Bool
noEndOrArrow c = (c /= ' ') && (c /= '>') && (c /= '<') && (c /= '/')

text :: Parser String
text = span noEndOrArrow

noArrow :: Parser String
noArrow =
  (:)
    <$> cond (/= '<')
    <*> span (/= '<')

--end of my code

natural :: Parser Integer
natural = do
  list <- reads <$> span isDigit
  case list of
    [(number, _)] -> return number
    _ -> abortParser "could not parse number"

newLineOrSpace c = if isSpace c || (c == '\n') then True else False

ws :: Parser String
ws = span isSpace

someWS :: Parser String
someWS = cond isSpace *> ws

atLeast :: (Char -> Bool) -> Parser String
atLeast pred = (:) <$> cond pred <*> span pred

nonWS :: Parser String
nonWS = span (not . isSpace)

idP :: Parser ()
idP = pure ()

orChain :: Parser a
orChain = abortParser "orChain parser invoked"

integer :: Parser Integer
integer =
  idP
    *> natural
    <|> negate
    <$> (char '-' *> ws *> natural)

separated :: Parser a -> Parser b -> Parser [b]
separated separator element =
  (:) <$> element <*> many (separator *> element) <|> pure []

defaultP :: a -> Parser a -> Parser a
defaultP def p = p <|> pure def

split :: String -> Parser b -> Parser [b]
split separator =
  separated (ws *> string separator <* ws)

list :: Parser b -> Parser [b]
list elementParser =
  char '[' *> ws *> split "," elementParser <* ws <* char ']'

intList :: Parser [Integer]
intList = list integer

intList' :: Parser [[Integer]]
intList' = list intList

end :: Parser ()
end = (*>) ws $
  Parser $ \case
    [] -> Right ([], ())
    _ -> abort "expected end of string"

class Parseable p where
  parser :: Parser p

instance Parseable Integer where
  parser = integer

instance Parseable Int where
  parser = fromIntegral <$> integer

instance Parseable Char where
  parser = nom

instance Parseable String where
  parser = nomAll

instance Parseable a => Parseable [a] where
  parser = list parser

instance Parseable Bool where
  parser = bool

parse :: Parseable p => String -> p
parse input =
  case runParser parser input of
    Left err -> error $ show err
    Right (_rest, value) -> value

main'' :: IO ()
main'' = do
  print $ runParser (char 'c') "cow-say moo"
  print $ runParser (ord <$> char 'c') "cows"
  print $ runParser (ord <$> char 'y' <* char 'x') "yxz"

  print $ runParser (string "world") "world, hello"

  print $ runParser bool "True and stuff"
  print $ runParser bool "False and stuff"
  print $ runParser bool "nothing and stuff"

  print $ runParser (span isDigit) "1234abcd321"
  print $ runParser natural "1234abcd321"
  print $ runParser natural "abcd321"

  print $ runParser (list bool) "[ True,   False , False  ] test"

  print $ runParser intList' "[ [12],  [ 54 , 23]  ]dsa"
  print $ runParser intList' "[ [12],  [ 54 , [23]  ]dsa"
--}