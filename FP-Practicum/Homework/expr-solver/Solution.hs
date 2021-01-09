module Solution where

import Control.Applicative (Alternative ((<|>)))
import ParserUtils (Parser (runParser), char, natural, ws)

data Еxpr a
  = Atom a
  | Add (Еxpr a) (Еxpr a)
  | Subtr (Еxpr a) (Еxpr a)
  | Mul (Еxpr a) (Еxpr a)
  | Div (Еxpr a) (Еxpr a)
  deriving (Show, Eq)

eval :: (Num a, Integral a) => Еxpr a -> Maybe a
eval (Atom n) = Just n
eval (Add left right) = (+) <$> eval left <*> eval right
eval (Subtr left right) = (-) <$> eval left <*> eval right
eval (Mul left right) = (*) <$> eval left <*> eval right
eval (Div left right) =
  let leftR = eval left
      rightR = eval right
   in if rightR == Just 0
        then Nothing
        else div <$> leftR <*> rightR

evaluateString :: String -> Maybe Integer
evaluateString str =
  let Right res = runParser parseString str
   in eval $ snd res

parseOperation :: Parser (Еxpr a -> Еxpr a -> Еxpr a)
parseOperation = do
  op <- char '+' <|> char '-' <|> char '*' <|> char '/'
  case op of
    '+' -> return Add
    '-' -> return Subtr
    '*' -> return Mul
    '/' -> return Div

parseExpr :: Parser (Еxpr Integer)
parseExpr = do
  left <- ws *> char '(' *> ws *> parseString <* ws
  op <- ws *> parseOperation <* ws
  right <- ws *> parseString <* ws <* char ')' <* ws
  return (op left right)

parseAtom :: Parser (Еxpr Integer)
parseAtom = do Atom <$> natural

parseString :: Parser (Еxpr Integer)
parseString = parseExpr <|> parseAtom