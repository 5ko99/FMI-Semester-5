module Types where

data Player = First | Second deriving (Eq)

instance Show Player where
  show First = "x"
  show Second = "o"

type Board = [[Maybe Player]]

type Position = (Int, Int)

data GameState
  = GameOver
      { getBoard :: Board,
        getWinner :: Maybe Player
      }
  | Turn
      { getBoard :: Board,
        getPlayer :: Player
      }
  deriving (Show)
