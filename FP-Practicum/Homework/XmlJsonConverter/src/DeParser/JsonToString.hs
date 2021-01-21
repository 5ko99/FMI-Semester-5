module DeParser.JsonToString where

import Data.JsonObject ( JsonValue(..) )

stringsToString :: [String] -> String
stringsToString [] = ""
stringsToString (x : xs) = res
  where
    res = ',' : x ++ stringsToString xs

jsonObjectToString :: [(String, JsonValue)] -> String
jsonObjectToString [] = ""
jsonObjectToString ((str, val) : xs) =
  '{' : '"' : str ++ "\": {" ++ jsonValueToString val ++ "} " ++ jsonObjectToString xs ++ "}"

jsonValueToString :: JsonValue -> String
jsonValueToString JsonNull = "null"
jsonValueToString (JsonBool b) = show b
jsonValueToString (JsonNumber n) = show n
jsonValueToString (JsonString str) = '"' : str ++ "\""
jsonValueToString (JsonArray arr) = "[" ++ drop 1 (stringsToString (map jsonValueToString arr)) ++ "]"
jsonValueToString (JsonObject obj) = jsonObjectToString obj