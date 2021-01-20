import XMLObject

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
  if str /= "xml"
    then
      "<" ++ str ++ attributesToString atr ++ ">"
        ++ stringsToString (map xmlToString xmlObj)
        ++ "</"
        ++ str
        ++ ">"
    else "<?xml version=\" 1.0 \" encoding=\" UTF -8 \" ?>"

xmlToString :: XMLObject -> String
xmlToString (Text text) = text
xmlToString (Element el) = tagElementToString el
