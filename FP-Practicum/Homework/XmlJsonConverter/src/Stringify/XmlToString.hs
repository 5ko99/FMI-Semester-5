module DeParser.XmlToString where

import Data.XMLObject
  ( Attribute,
    TagElement (TagElement),
    XMLObject (..),
  )
import Helper

attributeToString :: Attribute -> String
attributeToString (a, b) = ' ' : a ++ "=" ++ "\"" ++ b ++ "\""

attributesToStrings :: [Attribute] -> [String]
attributesToStrings = map attributeToString

stringsToString :: [String] -> String
stringsToString [] = ""
stringsToString (x : xs) = x ++ " " ++ stringsToString xs

attributesToString :: [Attribute] -> String
attributesToString = stringsToString . attributesToStrings

tagElementToString :: TagElement -> Either String String
tagElementToString (TagElement str atr xmlObj) = do
  others <- toEitherList (map xmlToString xmlObj)
  return $
    "<" ++ str ++ attributesToString atr ++ ">\n"
      ++ stringsToString others
      ++ "\n</"
      ++ str
      ++ ">"

xmlToString :: XMLObject -> Either String String
xmlToString (Text text) = Right text
xmlToString (Element el) = tagElementToString el
