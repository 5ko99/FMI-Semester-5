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

closingTagParser :: Parser String
closingTagParser =
  (\_ _ _ tagName _ _ -> "</" ++ tagName ++ ">")
    <$> ws
    <*> char '<'
    <*> char '/'
    <*> closingName
    <*> char '>'
    <*> ws

tagParser :: Parser XMLObject
tagParser =
  (\_ _ _ name _ attributes _ _ _ children _ _ _ -> Element $ TagElement name attributes children)
    <$> ws
    <*> char '<'
    <*> ws
    <*> text
    <*> ws
    <*> many attributeParser
    <*> ws
    <*> char '>'
    <*> ws
    <*> some xmlParser
    <*> ws
    <*> closingTagParser
    <*> ws

textParser :: Parser XMLObject
textParser =
  (\_ text _ -> Text text)
    <$> ws
    <*> noArrow
    <*> ws

emptyTagParser :: Parser XMLObject
emptyTagParser =
  (\_ _ name _ attributes _ _ _ _ _ -> Element $ TagElement name attributes [])
    <$> ws
    <*> char '<'
    <*> text
    <*> ws
    <*> many attributeParser
    <*> ws
    <*> char '>'
    <*> string "a"
    <*> closingTagParser
    <*> ws

xmlParser :: Parser XMLObject
xmlParser = tagParser <|> textParser

test = do
  actualContent <- readFile $ "test-files/" ++ "c" ++ ".xml"
  return $ runParser xmlParser actualContent