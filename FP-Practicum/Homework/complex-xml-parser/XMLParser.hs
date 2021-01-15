{-# LANGUAGE NamedFieldPuns #-}

module XMLParser where

import Control.Applicative
import Data.Char
import ParserUtils
import Prelude hiding (span)

type Attribute = (String, String)

data TagElement = TagElement
  { name :: String,
    attributes :: [Attribute],
    children :: [XMLObject]
  }
  deriving (Show, Read, Eq)

data XMLObject
  = Text String
  | Element TagElement
  deriving (Show, Read, Eq)

attributeParser :: Parser Attribute
attributeParser =
  (\_ name _ param _ -> (name, param))
    <$> ws
    <*> tag
    <*> char '='
    <*> stringLiteral
    <*> ws

closingTagParser :: String -> Parser String
closingTagParser name = do
  ws
  string "</"
  closingName <- closingName
  char '>'
  ws
  if name == closingName
    then return name
    else abortParser "Error!" 0

tagParser :: Parser XMLObject
tagParser = do
  name <- ws *> char '<' *> ws *> text <* ws
  attributes <- many attributeParser
  ws <* char '>' <* ws
  children <- many xmlParser
  ws *> closingTagParser name <* ws
  return $ Element $ TagElement name attributes children

textParser :: Parser XMLObject
textParser =
  Text
    <$> (ws *> noArrow <* ws)

xmlParser :: Parser XMLObject
xmlParser = tagParser <|> textParser

test :: IO (ParserResult XMLObject)
test = do
  actualContent <- readFile "test-files/a.xml"
  return $ runParser xmlParser actualContent