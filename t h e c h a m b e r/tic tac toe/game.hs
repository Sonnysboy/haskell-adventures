{-# LANGUAGE LambdaCase #-}

module TTT where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bool

{-
a board holds indices from 0->8 like so:
0 1 2
3 4 5
6 7 8
A cell can either be PlayerOne, PlayerTwo, or Free if nobody has taken it.
-}
newtype Board = Board [CellState]

hasWin :: Board -> Player -> Bool
hasWin (Board board) player
  | or ([row x | x <- [0, 3, 5]]) = True
  | or ([col x | x <- [0, 1, 2]]) = True
  | diag board = True
  | otherwise = False
  where
    row x =
      all
        ( \case
            (Taken p) -> p == player
            Free -> False
        )
        [board !! a | a <- [x .. x + 2]]
    col x =
      all
        ( \case
            (Taken p) -> p == player
            Free -> False
        )
        [board !! a | a <- map (+ x) [0, 3, 6]]
    diag board = all (\x -> x /= Free && x == head board) [board !! a | a <- [4, 8]] || all (\x -> x /= Free && x == (board !! 2)) [board !! a | a <- [4, 6]]

data Player = PlayerOne | PlayerTwo
  deriving (Eq)

instance Show Player where
  show PlayerOne = "Player One"
  show PlayerTwo = "Player Two"

data CellState = Taken Player | Free
  deriving (Eq)

instance Show CellState where
  show :: CellState -> String
  show (Taken PlayerOne) = "x"
  show (Taken PlayerTwo) = "o"
  show Free = "_"

instance Show Board where
  show :: Board -> String
  show (Board x) = unwords $ zipWith (\i x -> bool x ('\n' : x) (i /= 0 && i `mod` 3 == 0)) [0 ..] (map show x)

data GameState = Turn Player Board | Victory Player Board

-- newtype Move = Move (GameState -> Maybe (Board, GameState))

data MoveFailed = AlreadyTaken | InvalidSpace
  deriving (Show)

tupleToBoardSpot :: (Int, Int) -> Int
tupleToBoardSpot (1, 1) = 0
tupleToBoardSpot (1, 2) = 1
tupleToBoardSpot (1, 3) = 2
tupleToBoardSpot (2, 1) = 3
tupleToBoardSpot (2, 2) = 4
tupleToBoardSpot (2, 3) = 5
tupleToBoardSpot (3, 1) = 6
tupleToBoardSpot (3, 2) = 7
tupleToBoardSpot (3, 3) = 8

-- if Left, then nothing happened, i.e the space was taken.
-- so we can do like (1,1)
takeSpot :: Player -> Board -> (Int, Int) -> Either MoveFailed Board
takeSpot p (Board cells) tuple
  | index < 0 = Left InvalidSpace
  | index > length cells = Left InvalidSpace
  | otherwise = case cells !! index of
      (Taken _) -> Left AlreadyTaken
      Free -> replaceCell cells
  where
    index = tupleToBoardSpot tuple
    replaceCell = Right . Board . zipWith (\i val -> bool val (Taken p) (i == index)) [0 ..]

runGame :: GameState -> IO GameState
runGame (Turn player board) = do
  print board
  putStrLn "Please enter a tuple i.e (1,1)"
  coordinate <- fmap read getLine :: IO (Int, Int)
  let result = takeSpot player board coordinate
  case result of
    (Right board)
      | hasWin board player -> victory player board
      | otherwise -> runGame $ Turn (flipPlayer player) board
    (Left x) -> do
      print x
      runGame $ Turn player board
  where
    flipPlayer PlayerOne = PlayerTwo
    flipPlayer PlayerTwo = PlayerOne
    victory player board = do putStrLn $ show player ++ " Wins!\nFinal board:\n" ++ show board; return $ Victory player board
