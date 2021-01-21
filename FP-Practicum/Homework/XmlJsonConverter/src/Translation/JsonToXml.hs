module Translation.JsonToXml (jsonToXml) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Either
import Data.JsonObject
import Data.XMLObject

jsonListToXml :: [(String, JsonValue)] -> Either String [XMLObject]
jsonListToXml [] = Right []
jsonListToXml lst@((name, json@(JsonObject pair)) : xs) =
  Right $ Element (TagElement name attributes obj) : other
  where
    attributes = getAttributes pair
    Right other = jsonListToXml xs
    Right obj = jsonToXmlObjL json
jsonListToXml ((name, json) : xs)
  | name == "Text" = Right $ Text (fromRight "Error: Not a JsonString!" (getString json)) : others
  | head name /= '-' = Right $ Element (TagElement name [] obj) : others
  | otherwise = jsonListToXml xs
  where
    Right others = jsonListToXml xs
    Right obj = jsonToXmlObjL json

getString :: JsonValue -> Either String String
getString (JsonString str) = Right str
getString _ = Left "Error: Not a JsonString!"

jsonArrToXml :: [JsonValue] -> Either String [XMLObject]
jsonArrToXml arr =
  if all (== True) (isRight <$> mapped)
    then reverseListUnsafeAndCat mapped
    else Left ""
  where
    mapped = map jsonToXmlObjL arr

reverseListUnsafeAndCat :: [Either a [b]] -> Either a [b]
reverseListUnsafeAndCat [] = Right []
reverseListUnsafeAndCat (Right b : rest) =
  Right $
    b
      ++ fromRight [] (reverseListUnsafeAndCat rest)
reverseListUnsafeAndCat (Left a : _) = Left a

getAttributes :: [(String, JsonValue)] -> [Attribute]
getAttributes [] = []
getAttributes ((name, JsonString str) : xs)
  | head name == '-' = (drop 1 name, str) : getAttributes xs
  | otherwise = getAttributes xs
getAttributes (_ : xs) = getAttributes xs

jsonToXmlObjL :: JsonValue -> Either String [XMLObject]
jsonToXmlObjL JsonNull = Right [Text "null"]
jsonToXmlObjL (JsonString str) = Right [Text str]
jsonToXmlObjL (JsonBool b) = Right [Text $ show b]
jsonToXmlObjL (JsonNumber n) = Right [Text $ show n]
jsonToXmlObjL (JsonArray arr) = jsonArrToXml arr
jsonToXmlObjL (JsonObject arr) = jsonListToXml arr

jsonToXml :: JsonValue -> Either String XMLObject
jsonToXml (JsonObject [(name, json@(JsonObject jv))]) =
  Right $ Element (TagElement name attributes lst)
  where
    attributes = getAttributes jv
    Right lst = jsonToXmlObjL json
jsonToXml json =
  Right $ Element (TagElement "xml" atr lst)
  where
    atr = [("version=", "\" 1.0 \""), ("encoding=", "\" UTF -8 \"")]
    Right lst = jsonToXmlObjL json