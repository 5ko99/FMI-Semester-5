module JsonToXml where

import JsonObjectMod
import XMLObjectMod

jsonToXml :: JsonValue -> XMLObject
jsonToXml JsonNull = Text "null"
jsonToXml (JsonString str) = Text str
jsonToXml (JsonBool b) = Text $ show b
jsonToXml (JsonNumber n) = Text $ show n
jsonToXml (JsonArray arr) = Element $ TagElement "p" [] (toArray "p" arr) --TODO change this hardcoded p
    where
        toArray :: String -> [JsonValue] -> [XMLObject]
        toArray _ [] = []
        toArray s (x:xs) =  Element (TagElement s [] [Text $ show x]) : toArray s xs
jsonToXml (JsonObject arr) = Element (TagElement "main" [] (helper arr))
    where
        helper :: [(String, JsonValue)] -> [XMLObject]
        helper [] = []
        helper ((s, val):xs) = Element (TagElement s [] [jsonToXml val]) : helper xs