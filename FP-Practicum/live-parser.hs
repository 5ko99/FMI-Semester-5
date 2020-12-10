import Control.Applicative (Alternative ((<|>)))

type ParseError = String

type ParseResult a = Either ParseError (String,a)

newtype Parser a = Parser 
{ runParser :: String -> ParseResult a
}

nom :: Parse Char
nom = Parser  $ \input -> case input of
    [] -> Left "could not nom from empty string"
    (h:t) -> Right (t,h)
    
