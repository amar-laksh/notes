module TicTacToe
  ( Symbol (..),
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

-- Graphics section

-- Pure functions; we can test these exhaustively!
symGridRow :: Position -> Int -> Int -> [Position]
symGridRow (x, y) gridOffset numOfSymbols = do
  [(x + (gridOffset * row), y) | row <- [0 .. numOfSymbols - 1]]

symGridPositions :: Int -> Position -> Int -> [[Position]]
symGridPositions cellSize position numOfSymbols = do
  [symGridRow (originX, originY + (cellSize * column)) cellSize numOfSymbols | column <- columns]
  where
    -- We put the symbols in the middle of the cell
    symbolOffset = cellSize `div` 2
    originX = fst position + symbolOffset
    originY = snd position + symbolOffset
    columns = [0 .. numOfSymbols - 1]

-- Monadic functions
putStringAt :: Position -> String -> IO ()
putStringAt position symbol = do
  goto position
  putStr symbol

putSymbolsRow :: [Position] -> Int -> [Symbol] -> IO ()
putSymbolsRow positions offset symbols = do
  sequence_ [putStringAt position (toSymbol symbol) | (position, symbol) <- zip positions symbols]

putSymbolsGrid :: Int -> Int -> Position -> SymbolsGrid -> IO ()
putSymbolsGrid cellSize gridSize position gridSymbols = do
  sequence_ [putSymbolsRow rowPositions cellSize rowSymbols | (rowPositions, rowSymbols) <- zip gridPositions gridSymbols]
  where
    gridPositions = symGridPositions cellSize position gridSize

putLine :: Int -> [Int] -> (Position -> IO ()) -> IO ()
putLine coord offsets fn = do
  sequence_ [fn (offsetCoord, coord) | offsetCoord <- offsets]

putTicTacToeGrid :: Int -> Int -> Position -> SymbolsGrid -> IO ()
putTicTacToeGrid cellSize gridSize position gridSymbols = do
  sequence_ [putLine (originX + dx * cellSize) hOffsets hLine | dx <- gridLines]
  sequence_ [putLine (originY + dy * cellSize) vOffsets vLine | dy <- gridLines]
  putSymbolsGrid cellSize gridSize position gridSymbols
  goto (0, 0)
  where
    originX = fst position
    originY = snd position
    hOffsets = [originY .. originY + (cellSize * gridSize)]
    vOffsets = [originX .. originX + (cellSize * gridSize)]
    gridLines = [1 .. gridSize - 1]
    hLine (x, y) = putStringAt (y, x) "|"
    vLine (x, y) = putStringAt (x, y) "-"

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

data Symbol = O | B | X deriving (Eq, Ord, Show)

type SymbolsGrid = [[Symbol]]

next :: Symbol -> Symbol
next O = X
next B = B
next X = O

empty :: SymbolsGrid
empty = replicate gridSize (replicate gridSize B)

isFull :: SymbolsGrid -> Bool
isFull = notElem B . concat

turn :: SymbolsGrid -> Symbol
turn grid = if os <= xs then O else X
  where
    ps = concat grid
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)

diag :: SymbolsGrid -> [Symbol]
diag grid = [grid !! n !! n | n <- [0 .. gridSize - 1]]

winFor :: Symbol -> SymbolsGrid -> Bool
winFor p grid = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = grid
    cols = transpose grid
    dias = [diag grid, diag (map reverse grid)]

won :: SymbolsGrid -> Bool
won grid = winFor O grid || winFor X grid

toSymbol :: Symbol -> String
toSymbol O = "⭕"
toSymbol X = "❌"
toSymbol B = "  "

putGrid :: SymbolsGrid -> IO ()
putGrid = putTicTacToeGrid cellSize gridSize origin

validMove :: SymbolsGrid -> Int -> Bool
validMove grid index = index >= 0 && index < gridSize ^ 2 && concat grid !! index == B

move :: SymbolsGrid -> Int -> Symbol -> [SymbolsGrid]
move grid index symbol = [chop gridSize (xs ++ [symbol] ++ ys) | validMove grid index]
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

run :: SymbolsGrid -> Symbol -> IO ()
run g p = do
  -- Reset screen
  clearScn
  goto (0, 0)
  putGrid g
  run' g p

run' :: SymbolsGrid -> Symbol -> IO ()
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
