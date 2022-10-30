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
import Life (clearScn, goto')

-- Graphics section

-- Each cell size in the grid
gridCellSize :: Int
gridCellSize = 3

putStringAt :: Position -> String -> IO ()
putStringAt pos symbol = do
  goto' pos
  putStr symbol

putPlayerRow :: Position -> Int -> [Player] -> IO ()
putPlayerRow (x, y) offset players = do
  sequence_ [putStringAt (x + (offset * row), y) (toSymbol player) | (row, player) <- zip rows players]
  where
    rows = [0 .. (length players)]

putPlayerGrid :: Int -> Position -> Grid -> IO ()
putPlayerGrid cellSize pos grid = do
  sequence_ [putPlayerRow (originX, originY + (cellSize * column)) cellSize row | (column, row) <- zip gridColumns grid]
  where
    symbolOffset = cellSize `div` 2
    originX = fst pos + symbolOffset
    originY = snd pos + symbolOffset
    gridColumns = [0 .. (length grid)]

putLine :: Int -> Int -> Int -> Int -> ((Int, Int) -> IO ()) -> IO ()
putLine cellSize gridSize y offset fn = do
  sequence_ [fn (x, y) | x <- [offset .. offset + (cellSize * gridSize)]]

putTicTacToeGrid :: Int -> Int -> Position -> Grid -> IO ()
putTicTacToeGrid cellSize gridSize pos grid = do
  sequence_ [putLine cellSize gridSize (originX + dx * cellSize) originY putHLine | dx <- gridLines]
  sequence_ [putLine cellSize gridSize (originY + dy * cellSize) originX putVLine | dy <- gridLines]
  putPlayerGrid cellSize pos grid
  goto' (0, 0)
  where
    originX = fst pos
    originY = snd pos
    gridLines = [1 .. gridSize - 1]
    putHLine (x, y) = putStringAt (y, x) "|"
    putVLine (x, y) = putStringAt (x, y) "-"

-- TODO : Fix this to be correct
moveGrid' :: Position -> IO ()
moveGrid' (x, y) = do
  goto' (0, 0)
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

origin = (50, 10)

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

interleave xs ys = concatMap (\(x, y) -> [x, y]) (zip xs ys)

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

toPosition :: Int -> Int -> Position
toPosition turn offset = (turn - (15 * (offset * gridSize)), gridSize + (offset * 2))

fromPosition :: Position -> Position
fromPosition (x, y) = (turn, offset)
  where
    offset = (y - gridSize) `div` 2
    turn = x + (15 * (offset * gridSize))

toOffset :: Int -> Int
toOffset turn = (turn `div` 15) `div` gridSize

nextTurn :: Int -> Int
nextTurn turn = turn + 15

prevTurn :: Int -> Int
prevTurn turn = turn - 15

processInput :: Int -> IO (Int, Position)
processInput turn = do
  input <- readLine'
  let ofst = toOffset turn
  let pos = toPosition turn ofst
  if input == "\ESC[C"
    then do
      goto' pos
      let nextOfst = toOffset (nextTurn turn)
      let nextPos = toPosition (nextTurn turn) nextOfst
      if nextOfst == gridSize && fst nextPos == 10
        then do
          (turn, pos) <- processInput 10
          return (turn, pos)
        else do
          (turn, pos) <- processInput (nextTurn turn)
          return (turn, pos)
    else
      if input == " "
        then do
          goto' pos
          return (turn, pos)
        else do
          return (-1, pos)

getNat :: Int -> IO (Int, Position)
getNat turn = do
  if toOffset turn == gridSize && fst (toPosition turn (toOffset turn)) == 10
    then do
      (turn, pos) <- processInput 10
      return (turn, pos)
    else do
      (turn, pos) <- processInput turn
      return (turn, pos)

tictactoe :: IO ()
tictactoe = run empty O (10, 3)

run :: Grid -> Player -> Position -> IO ()
run g p pos = do
  -- Reset screen
  clearScn
  goto' (0, 0)
  putGrid g
  goto' pos
  run' g p (nextTurn (fst (fromPosition pos)))

run' :: Grid -> Player -> Int -> IO ()
run' g p turn
  | winFor O g = printMsg (toPosition turn (toOffset turn)) "Player O wins!\n"
  | winFor X g = printMsg (toPosition turn (toOffset turn)) "Player X wins!\n"
  | isFull g = printMsg (toPosition turn (toOffset turn)) "It's a draw!\n"
  | otherwise = do
      (turn, pos) <- getNat turn
      let t = turn `div` 15
      let index
            | t > 0 = t - 1
            | otherwise = (gridSize * gridSize) - 1
      case move g index p of
        [] -> do
          -- printMsg pos "ERROR: Invalid move"
          run' g p turn
        [g'] -> do
          run g' (next p) pos

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

printMsg :: Position -> String -> IO ()
printMsg pos msg = do
  goto' (gridSize * gridSize * 10, 1)
  putStrLn msg
  goto' pos
