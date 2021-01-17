import Control.Applicative
import Data.Char
import ParserUtils

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Float
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

jsonNull :: Parser JsonValue
jsonNull = do
  ws *> string "null" <* ws
  return JsonNull

jsonBool :: Parser JsonValue
jsonBool = JsonBool <$> bool

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> float

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (char '[' *> ws *> elements <* ws <* char ']')
  where
    elements = sepBy (ws *> char ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject
    <$> ( char '{'
            *> ws
            *> sepBy (ws *> char ',' <* ws) pair
            <* ws
            <* char '}'
        )
  where
    pair =
      (\key _ value -> (key, value))
        <$> stringLiteral
        <*> (ws *> char ':' <* ws)
        <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

--test

parseFile :: FilePath -> IO (ParserResult JsonValue)
parseFile fileName = do
  input <- readFile fileName
  return $ runParser jsonValue input