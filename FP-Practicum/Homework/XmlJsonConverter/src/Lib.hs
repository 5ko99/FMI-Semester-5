module Lib
  ( someFunc,
  )
where

import Data.JsonObject
import Data.XMLObject

import Translation.JsonToXml (jsonToXml)

main :: IO ()
main = do
  print $
    jsonToXml $
      JsonObject
        [ ( "quiz",
            JsonObject
              [ ( "sport",
                  JsonObject
                    [ ( "q1",
                        JsonObject
                          [ ( "question",
                              JsonString "Which one is correct team name in NBA?"
                            ),
                            ("options", JsonArray [JsonString "New York Bulls", JsonString "Los Angeles Kings", JsonString "Golden State Warriros", JsonString "Huston Rocket"]),
                            ("answer", JsonString "Huston Rocket")
                          ]
                      )
                    ]
                ),
                ("maths", JsonObject [("q1", JsonObject [("question", JsonString "5 + 7 = ?"), ("options", JsonArray [JsonString "10", JsonString "11", JsonString "12", JsonString "13"]), ("answer", JsonString "12")]), ("q2", JsonObject [("question", JsonString "12 - 8 = ?"), ("options", JsonArray [JsonString "1", JsonString "2", JsonString "3", JsonString "4"]), ("answer", JsonString "4")])])
              ]
          )
        ]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
