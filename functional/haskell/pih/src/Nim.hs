module Nim
  ( putRow,
    Board (..),
    putBoard,
    play,
  )
where

import Data.Char
import Data.Foldable

next :: Int -> Int
next 1 = 2
next 2 = 1
next _ = 1

type Board = [Int]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard list = do
  forM_ putBoardValues $ \(r, n) -> do
    putRow r n
  where
    putBoardValues = [(k, v) | (k, v) <- zip [1 ..] list]

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  ch <- getChar
  newline
  if isDigit ch
    then return (digitToInt ch)
    else do
      -- putStrLn "ERROR: Invalid Digit"
      getDigit prompt

play :: Board -> Int -> IO ()
play board player = do
  newline
  putBoard board
  if finished board
    then do
      newline
      putStr "Player: "
      print (next player)
      putStrLn " wins!!"
    else do
      newline
      putStr "Player: "
      print player
      row <- getDigit "Enter a row number: "
      num <- getDigit "Stars to remove: "
      if valid board row num
        then play (move board row num) (next player)
        else do
          newline
          putStrLn "ERROR: Invalid Move"
          play board player
