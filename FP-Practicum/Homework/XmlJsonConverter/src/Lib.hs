module Lib
  ( someFunc,
  )
where

import Data.JsonObject
import Data.XMLObject
import qualified Translation.ToJson as ToJson
import qualified Translation.ToXml as ToXml

someFunc :: IO ()
someFunc = putStrLn "someFunc"
