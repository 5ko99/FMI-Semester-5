module DeParser.XmlToString where

import Data.XMLObject
  ( Attribute,
    TagElement (TagElement),
    XMLObject (..),
  )

attributeToString :: Attribute -> [Char]
attributeToString (a, b) = ' ' : a ++ "=" ++ "\"" ++ b ++ "\""

attributesToStrings :: [Attribute] -> [String]
attributesToStrings = map attributeToString

stringsToString :: [String] -> String
stringsToString [] = ""
stringsToString (x : xs) = x ++ " " ++ stringsToString xs

attributesToString :: [Attribute] -> String
attributesToString = stringsToString . attributesToStrings

tagElementToString :: TagElement -> String
tagElementToString (TagElement str atr xmlObj) =
  "<" ++ str ++ attributesToString atr ++ ">"
    ++ stringsToString (map xmlToString xmlObj)
    ++ "</"
    ++ str
    ++ ">"

xmlToString :: XMLObject -> String
xmlToString (Text text) = text
xmlToString (Element el) = tagElementToString el
