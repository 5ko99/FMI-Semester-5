module JsonToXml where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe
import JsonObjectMod
import XMLObjectMod

jsonListToXml :: [(String, JsonValue)] -> [XMLObject]
jsonListToXml [] = []
jsonListToXml lst@((name, json@(JsonObject pair)) : xs) =
  Element (TagElement name attributes (jsonToXmlObjL json)) : jsonListToXml xs
  where
    attributes = getAttributes pair
jsonListToXml ((name, json) : xs)
  | name == "Text" = Text (getString json) : jsonListToXml xs
  | head name /= '-' = Element (TagElement name [] (jsonToXmlObjL json)) : jsonListToXml xs
  | otherwise = jsonListToXml xs

getString :: JsonValue -> String
getString (JsonString str) = str

jsonArrToXml :: [JsonValue] -> [XMLObject]
jsonArrToXml arr = concat mapped
  where
    mapped = map jsonToXmlObjL arr

getAttributes :: [(String, JsonValue)] -> [Attribute]
getAttributes [] = []
getAttributes ((name, JsonString str) : xs)
  | head name == '-' = (drop 1 name, str) : getAttributes xs
  | otherwise = getAttributes xs
getAttributes (_ : xs) = getAttributes xs

jsonToXmlObjL :: JsonValue -> [XMLObject]
jsonToXmlObjL JsonNull = [Text "null"]
jsonToXmlObjL (JsonString str) = [Text str]
jsonToXmlObjL (JsonBool b) = [Text $ show b]
jsonToXmlObjL (JsonNumber n) = [Text $ show n]
jsonToXmlObjL (JsonArray arr) = jsonArrToXml arr
jsonToXmlObjL (JsonObject arr) = jsonListToXml arr

jsonToXml :: JsonValue -> XMLObject
jsonToXml json = Element (TagElement "xml" [] lst)
  where
    lst = jsonToXmlObjL json