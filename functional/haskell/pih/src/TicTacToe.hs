module TicTacToe
  ( Player (..),
    toSymbol,
    putGrid,
    getNat,
    tictactoe,
    winFor,
    moveGrid,
    putTicTacToeGrid,
  )
where

import Chapter10 (getChar', readLine')
import Control.Concurrent
import Data.Char (digitToInt)
import Data.List
import Graphics (clearScn, goto)

-- import Life (goto)

-- Graphics section

-- Each cell size in the grid
gridCellSize :: Int
gridCellSize = 3

putStringAt :: Position -> String -> IO ()
putStringAt pos symbol = do
  goto pos
  putStr symbol

getPlayerRow :: Position -> Int -> [Player] -> [Position]
getPlayerRow (x, y) offset players = do
  [(x + (offset * row), y) | row <- rows]
  where
    rows = [0 .. (length players)]

getPlayerGrid :: Int -> Position -> Grid -> [[Position]]
getPlayerGrid cellSize pos grid = do
  [getPlayerRow (originX, originY + (cellSize * column)) cellSize row | (column, row) <- zip gridColumns grid]
  where
    symbolOffset = cellSize `div` 2
    originX = fst pos + symbolOffset
    originY = snd pos + symbolOffset
    gridColumns = [0 .. (length grid)]

putPlayerRow :: Position -> Int -> [Player] -> IO ()
putPlayerRow (x, y) offset players = do
  sequence_ [putStringAt pos (toSymbol player) | (pos, player) <- zip positions players]
  where
    positions = getPlayerRow (x, y) offset players

putPlayerGrid :: Int -> Position -> Grid -> IO ()
putPlayerGrid cellSize pos grid = do
  sequence_ [putPlayerRow (head playerRows) cellSize row | (playerRows, row) <- zip playerGrid grid]
  where
    playerGrid = getPlayerGrid cellSize pos grid

putLine :: Int -> Int -> Int -> Int -> ((Int, Int) -> IO ()) -> IO ()
putLine cellSize gridSize y offset fn = do
  sequence_ [fn (x, y) | x <- [offset .. offset + (cellSize * gridSize)]]

putTicTacToeGrid :: Int -> Int -> Position -> Grid -> IO ()
putTicTacToeGrid cellSize gridSize pos grid = do
  sequence_ [putLine cellSize gridSize (originX + dx * cellSize) originY hLine | dx <- gridLines]
  sequence_ [putLine cellSize gridSize (originY + dy * cellSize) originX vLine | dy <- gridLines]
  putPlayerGrid cellSize pos grid
  goto (0, 0)
  where
    originX = fst pos
    originY = snd pos
    gridLines = [1 .. gridSize - 1]
    hLine (x, y) = putStringAt (y, x) "|"
    vLine (x, y) = putStringAt (x, y) "-"

--
-- takePlayerFromGrid:: Int -> Int -> Position -> IO (Grid)
-- takePlayerFromGrid cellSize gridSize pos = do
--   let g =
--   return Grid
--

-- TODO : Fix this to be correct
moveGrid' :: Position -> IO ()
moveGrid' (x, y) = do
  goto (0, 0)
  threadDelay 20000
  clearScn
  putTicTacToeGrid 5 3 (x, y) [[O, O, X], [X, X, O], [X, B, X]]

moveGrid :: Position -> Position -> IO ()
moveGrid origin dest = do
  if (fst origin /= fst dest) && (snd origin /= snd dest)
    then do
      if (fst dest < fst origin) || (snd dest < snd origin)
        then do
          sequence_ [moveGrid' (x, y) | (x, y) <- zip (reverse inversePathX) (reverse inversePathY)]
        else do
          sequence_ [moveGrid' (x, y) | (x, y) <- zip pathX pathY]
    else do
      if (fst dest < fst origin) || (snd dest < snd origin)
        then do
          sequence_ [moveGrid' (x, y) | x <- reverse inversePathX, y <- reverse inversePathY]
        else do
          sequence_ [moveGrid' (x, y) | x <- pathX, y <- pathY]
  where
    pathX = [fst origin .. fst dest]
    pathY = [snd origin .. snd dest]
    inversePathX = [fst dest .. fst origin]
    inversePathY = [snd dest .. snd origin]

-- Game section
--
-- Game config
gridSize = 3

cellSize = 6

origin = (30, 10)

type Position = (Int, Int)

data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate gridSize (replicate gridSize B)

isFull :: Grid -> Bool
isFull = notElem B . concat

turn :: Grid -> Player
turn grid = if os <= xs then O else X
  where
    ps = concat grid
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)

diag :: Grid -> [Player]
diag grid = [grid !! n !! n | n <- [0 .. gridSize - 1]]

winFor :: Player -> Grid -> Bool
winFor p grid = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = grid
    cols = transpose grid
    dias = [diag grid, diag (map reverse grid)]

won :: Grid -> Bool
won grid = winFor O grid || winFor X grid

toSymbol :: Player -> String
toSymbol O = "⭕"
toSymbol X = "❌"
toSymbol B = "  "

putGrid :: Grid -> IO ()
putGrid = putTicTacToeGrid cellSize gridSize origin

validMove :: Grid -> Int -> Bool
validMove grid index = index >= 0 && index < gridSize ^ 2 && concat grid !! index == B

move :: Grid -> Int -> Player -> [Grid]
move grid index player = [chop gridSize (xs ++ [player] ++ ys) | validMove grid index]
  where
    (xs, B : ys) = splitAt index (concat grid)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

processInput :: IO Int
processInput = do
  input <- readLine'
  return 0

getNat :: IO Int
getNat = do
  processInput

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  -- Reset screen
  clearScn
  goto (0, 0)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | winFor O g = printMsg "Player O wins!\n"
  | winFor X g = printMsg "Player X wins!\n"
  | isFull g = printMsg "It's a draw!\n"
  | otherwise = do
      -- TODO:  make a getNat that works with arrows and returns the index
      turn <- getNat
      case move g turn p of
        [] -> do
          -- printMsg pos "ERROR: Invalid move"
          run' g p
        g' -> do
          run (head g') (next p)

printMsg :: String -> IO ()
printMsg msg = do
  goto (gridSize * gridSize * 10, 1)
  putStrLn msg
