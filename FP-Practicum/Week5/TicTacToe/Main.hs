module Main where

import Logic
import Rendering
import Text.Read (readMaybe)
import Types

loop :: GameState -> IO ()
loop g@GameOver {} = putStrLn $ showGame g
loop gs = do
  putStrLn $ showGame gs
  move <- getLine
  let parsedMove = readMaybe move :: Maybe Position
   in case parsedMove of
        Nothing -> do
          putStrLn "Invalid input, try again!"
          loop gs
        Just position ->
          case getNextState gs position of
            Nothing -> do
              putStrLn "Invalid, move!"
              loop gs
            Just nextGameState -> loop nextGameState

main :: IO ()
main = do
  loop firstTurn
