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
  (\_ name _ param -> (name, param))
    <$> ws
    <*> tag
    <*> char '='
    <*> stringLiteral


-- xmlParser :: Parser XMLObject
-- xmlParser =
--   XMLObject
--     <$> ( char '<'
--             <*> stringLiteral name
--             <*> someWS
--             <*> sepBy (ws *> char ' ' <* ws) pair
--             <*> char '>'
--             <*> someWS
--             <*> parserA
--             <* someWS
--             <*> char '<'
--             <*> char '/'
--             *> stringLiteral
--             <* char '>'
--         )
--   where
--     pair =
--       (\key _ value -> (key, value))
--         <$> stringLiteral
--         <*> char '='
--         <*> stringLiteral
