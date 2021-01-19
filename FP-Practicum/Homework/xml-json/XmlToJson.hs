module XmlToJson where

import JsonObjectMod
import XMLObjectMod

attributeToJson :: Attribute -> (String, JsonValue)
attributeToJson (name, val) = ('-' : name, JsonString val)

makePairs :: JsonValue -> (String, JsonValue)
makePairs (JsonObject [(name, obj)]) = (name, obj)

xmlToJson :: XMLObject -> JsonValue
xmlToJson (Text t) = JsonObject [("Text", JsonString t)]
xmlToJson (Element (TagElement name atr children)) =
  JsonObject [(name, JsonObject combined)]
  where
    atrParsed, combined :: [(String, JsonValue)]
    atrParsed = map attributeToJson atr
    childrenParsed :: [JsonValue]
    childrenParsed = map xmlToJson children
    combined = atrParsed ++ map makePairs childrenParsed

main :: IO ()
main = do
  print $
    xmlToJson $
      Element
        ( TagElement
            { name = "body",
              attributes =
                [("onlod", "alabal()"), ("src", "test/")],
              children =
                [ Element
                    ( TagElement
                        { name = "p",
                          attributes =
                            [("style", "background: red")],
                          children = [Text "test"]
                        }
                    ),
                  Element (TagElement {name = "p", attributes = [], children = []}),
                  Element (TagElement {name = "textarea", attributes = [], children = []}),
                  Element
                    ( TagElement
                        { name = "div",
                          attributes = [],
                          children =
                            [Element (TagElement {name = "p", attributes = [], children = [Text "test"]})]
                        }
                    )
                ]
            }
        )
