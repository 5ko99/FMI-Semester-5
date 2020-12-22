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
  (\name _ param -> (name, param))
    <$> tag
    <*> char '='
    <*> stringLiteral

closingTagParser :: Parser String
closingTagParser =
  (\_ _ tagName _ -> "</" ++ tagName ++ ">")
    <$> char '<'
    <*> char '/'
    <*> closingName
    <*> char '>'

tagParser :: Parser XMLObject
tagParser =
  (\_ _ name _ attributes _ _ children _ _ _ -> Element $ TagElement name attributes children)
    <$> ws
    <*> char '<'
    <*> text
    <*> ws
    <*> many attributeParser
    <*> ws
    <*> char '>'
    <*> some xmlParser
    <*> ws
    <*> closingTagParser
    <*> ws

--to Change
textParser :: Parser XMLObject
textParser =
  (\text -> Text text)
    <$> string "ad"

xmlParser :: Parser XMLObject
xmlParser = textParser <|> tagParser

--xmlParser = textParser <|> tagParser
