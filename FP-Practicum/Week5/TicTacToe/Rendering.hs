{-# LANGUAGE NamedFieldPuns #-}

module Rendering where

import Data.List (intersperse)
import Types

showBoard :: Board -> String
showBoard board =
  concat
    [ intersperse '|' $ concatMap (maybe " " show) r ++ "\n"
      | r <- board
    ]

showGame :: GameState -> String
showGame GameOver {getBoard = board, getWinner = winner} =
  showBoard board
    ++ "Game Over!\n"
    ++ label
  where
    label = case winner of
      Nothing -> "It's a draw"
      Just winner -> "Player " ++ show winner ++ " won!"
showGame Turn {getBoard = board, getPlayer = player} =
  showBoard board ++ playerLabel
  where
    playerLabel = "Player " ++ show player ++ " select your move:"

firstTurn =
  Turn
    { getPlayer = First,
      getBoard = replicate 3 $ replicate 3 Nothing
    }