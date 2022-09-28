module Life
  ( clearScn,
    Pos (..),
    Board (..),
    writeAt,
    goto',
    playLife,
    convertToBoard,
    isPrime,
    clearScn,
    printGotos,
  )
where

import Control.Concurrent

type Pos = (Int, Int)

type Board = [Pos]

width = 100

height = 50

cellShape = "🔥"

isPrime :: Int -> Bool
isPrime k = (k > 1) && null [x | x <- [2 .. k - 1], k `mod` x == 0]

initialBoard = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)] ++ convertToBoard (map (+ 2) [1 .. 10]) ++ convertToBoard (map (+ 4) [20 .. 30])

convertToBoard :: [Int] -> Board
convertToBoard seq = zip seq (tail seq)

clearScn :: IO ()
clearScn = putStr "\ESC[2J"

writeAt :: Pos -> String -> IO ()
writeAt position string = do
  goto' position
  putStr string

goto' :: Pos -> IO ()
goto' (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

printGotos :: Int -> Int -> IO ()
printGotos x y = do
  goto' (x, y)
  putChar '*'
  threadDelay 100

printBoard :: Board -> IO ()
printBoard board = sequence_ [writeAt position cellShape | position <- board]

isAlive :: Board -> Pos -> Bool
isAlive board position = position `elem` board

isEmpty :: Board -> Pos -> Bool
isEmpty board position = not (isAlive board position)

neighbours :: Pos -> [Pos]
neighbours (x, y) =
  map
    wrapAroundEdges
    [ (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1)
    ]

aliveNeighbours :: Board -> Pos -> Int
aliveNeighbours board = length . filter (isAlive board) . neighbours

emptyNeighbours :: Board -> Pos -> Int
emptyNeighbours board = length . filter (isEmpty board) . neighbours

wrapAroundEdges :: Pos -> Pos
wrapAroundEdges (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

birthsFrom :: Board -> Board
birthsFrom board =
  [ position | position <- rmdups (concatMap neighbours board), isEmpty board position, aliveNeighbours board position >= 2
  ]

survivoursOf :: Board -> Board
survivoursOf board = [position | position <- board, isAlive board position, aliveNeighbours board position `elem` [1, 2, 4]]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

nextGeneration :: Board -> Board
nextGeneration board = survivoursOf board ++ birthsFrom board

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

life :: Board -> IO ()
life board = do
  clearScn
  printBoard board
  wait 50000
  life (nextGeneration board)

playLife = life initialBoard
