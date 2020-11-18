module Logic where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Types

isValidPosition :: Board -> Position -> Bool
isValidPosition board (x, y) = isNothing $ board !! y !! x

swapListVal :: a -> Int -> [a] -> [a]
swapListVal el pos list =
  take pos list ++ [el] ++ drop (pos + 1) list

getNextBoard :: Board -> Player -> Position -> Board
getNextBoard board player (x, y) =
  swapListVal updatedRow y board
  where
    selectedRow = board !! y
    updatedRow = swapListVal (Just player) x selectedRow

isRowWin :: Board -> Player -> Bool
isRowWin board player = or [all (== Just player) r | r <- board]

isColWin :: Board -> Player -> Bool
isColWin board = isRowWin (transpose board)

isDiagonalWin :: Board -> Player -> Bool
isDiagonalWin board player = helper (0, 0) True || helper (2, 0) False
  where
    helper (2, 2) True = if (board !! 2 !! 2) == Just player then True else False
    helper (x, y) True = if (board !! x !! y) == Just player then helper (x + 1, y + 1) True else False
    helper (0, 2) False = if (board !! 0 !! 2) == Just player then True else False
    helper (x, y) False = if ((board !! x !! y) == Just player) then helper (x + (-1), y + 1) False else False

isPlayerWinning :: Board -> Player -> Bool
isPlayerWinning board player =
  isRowWin board player || isColWin board player || isDiagonalWin board player

isBoardFull :: Board -> Bool
isBoardFull board = not $ any isNothing $ concat board

getNextState :: GameState -> Position -> Maybe GameState
getNextState GameOver {} _ = Nothing
getNextState Turn {getBoard = board, getPlayer = player} pos
  | not $ isValidPosition board pos = Nothing
  | isPlayerWinning nextBoard First = gameOver $ Just First
  | isPlayerWinning nextBoard Second = gameOver $ Just Second
  | isBoardFull board = gameOver Nothing
  | otherwise = Just Turn {getBoard = nextBoard, getPlayer = nextPlayer}
  where
    nextBoard = getNextBoard board player pos
    gameOver maybePlayer = Just GameOver {getBoard = nextBoard, getWinner = maybePlayer}
    nextPlayer = if player == First then Second else First