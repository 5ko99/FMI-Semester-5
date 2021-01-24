module DeParser.JsonToString where

import Data.JsonObject (JsonValue (..))
import Helper

stringsToString :: [String] -> String
stringsToString [] = ""
stringsToString (x : xs) = res
  where
    res = ',' : x ++ stringsToString xs

jsonObjectToString :: [(String, JsonValue)] -> Either String String
jsonObjectToString [] = Right ""
jsonObjectToString ((str, val) : xs) = do
  jsonValueString <- jsonValueToString val
  jsonObjectsString <- jsonObjectToString xs
  return $ "{\n\"" ++ str ++ "\": {\n" ++ jsonValueString ++ "}\n" ++ jsonObjectsString ++ "}"

jsonValueToString :: JsonValue -> Either String String
jsonValueToString JsonNull = Right "null"
jsonValueToString (JsonBool b) = Right $ show b
jsonValueToString (JsonNumber n) = Right $ show n
jsonValueToString (JsonString str) = Right $ '"' : str ++ "\""
jsonValueToString (JsonArray arr) = do
  stringArr <- toEitherList (map jsonValueToString arr)
  return $ "[\n" ++ drop 1 (stringsToString stringArr) ++ "]\n" --to skip the firs comma.
jsonValueToString (JsonObject obj) = jsonObjectToString obj