-- TODO:
-- Get blank symbol positions
-- use those positions to move around to the nearest blank for a specific direction in the grid
-- the above can be invoked using arrow/hjkl keys or any other keypress

module TicTacToe
  ( Symbol (..),
    toSymbol,
    putGrid,
    getNat,
    tictactoe,
    moveGrid,
    putTicTacToeGrid,
    positionsForRow,
    positionsForGrid,
    positionsOf,
  )
where

import Chapter10 (readLine')
import Chapter5 (positions')
import Control.Concurrent
import Data.Char (isDigit)
import Data.List (transpose)
import Graphics (clearScn, goto)

-- Graphics section

-- Pure functions; we can test these exhaustively!
positionsForRow :: Position -> Int -> Int -> [Position]
positionsForRow (x, y) rowOffset rowLength = do
  [(x + (rowOffset * row), y) | row <- [0 .. rowLength - 1]]

positionsForGrid :: Position -> Int -> Int -> [[Position]]
positionsForGrid origin gridSize cellSize = do
  [positionsForRow (originX, originY + (cellSize * column)) cellSize gridSize | column <- columns]
  where
    -- We put the symbols in the middle of the cell
    symbolOffset = cellSize `div` 2
    originX = fst origin + symbolOffset
    originY = snd origin + symbolOffset
    columns = [0 .. gridSize - 1]

-- Monadic functions
putStringAt :: Position -> String -> IO ()
putStringAt position symbol = do
  goto position
  putStr symbol

putSymbolsRow :: [Position] -> [Symbol] -> IO ()
putSymbolsRow positions symbols = do
  sequence_ [putStringAt position (toSymbol symbol) | (position, symbol) <- zip positions symbols]

putSymbolsGrid :: Position -> SymbolsGrid -> Int -> Int -> IO ()
putSymbolsGrid origin gridSymbols gridSize cellSize = do
  sequence_ [putSymbolsRow rowPositions rowSymbols | (rowPositions, rowSymbols) <- zip gridPositions gridSymbols]
  where
    gridPositions = positionsForGrid origin gridSize cellSize

putLine :: Int -> [Int] -> (Position -> IO ()) -> IO ()
putLine coord offsets fn = do
  sequence_ [fn (offsetCoord, coord) | offsetCoord <- offsets]

putTicTacToeGrid :: Int -> Int -> Position -> SymbolsGrid -> IO ()
putTicTacToeGrid cellSize gridSize origin gridSymbols = do
  sequence_ [putLine (originX + dx * cellSize) hOffsets putHLine | dx <- gridLines]
  sequence_ [putLine (originY + dy * cellSize) vOffsets putVLine | dy <- gridLines]
  putSymbolsGrid origin gridSymbols gridSize cellSize
  where
    originX = fst origin
    originY = snd origin
    hOffsets = [originY .. originY + (cellSize * gridSize)]
    vOffsets = [originX .. originX + (cellSize * gridSize)]
    gridLines = [1 .. gridSize - 1]
    putHLine (x, y) = putStringAt (y, x) "|"
    putVLine (x, y) = putStringAt (x, y) "-"

-- TODO : Fix this to be correct
moveGrid' :: Position -> IO ()
moveGrid' (x, y) = do
  threadDelay 20000
  clearScn
  putTicTacToeGrid 5 3 (x, y) [[O, O, X], [X, X, O], [X, B, X]]
  goto (0, 0)

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

treeDepth = 9

data Symbol = O | B | X deriving (Eq, Ord, Show)

data GameTree t = Node t [GameTree t] deriving (Show)

type Position = (Int, Int)

type SymbolsGrid = [[Symbol]]

type PositionsGrid = [[Position]]

type SymbolsMap = [[(Symbol, Position)]]

-- Computer Part Starts --------------

gametree :: SymbolsGrid -> Symbol -> GameTree SymbolsGrid
gametree grid symbol = Node grid [gametree node (next symbol) | node <- moves grid symbol]

moves :: SymbolsGrid -> Symbol -> [SymbolsGrid]
moves grid symbol
  | won grid = []
  | full grid = []
  | otherwise = concat [move grid index symbol | index <- [0 .. ((gridSize ^ 2) - 1)]]

prune :: Int -> GameTree t -> GameTree t
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

minimax :: GameTree SymbolsGrid -> GameTree (SymbolsGrid, Symbol)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g trees)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, minimum ps) ts'
  | otherwise = Node (g, minimum ps) ts'
  where
    ts' = map minimax trees
    ps = [p | Node (_, p) _ <- ts']

bestmove :: SymbolsGrid -> Symbol -> SymbolsGrid
bestmove grid symbol = head [node | Node (node, symbol') _ <- trees, symbol' == best]
  where
    tree = prune treeDepth (gametree grid symbol)
    Node (_, best) trees = minimax tree

-- Computer Part Ends --------------

positionsOf :: Symbol -> SymbolsMap -> PositionsGrid
positionsOf symbol symbolMap = do
  [map snd (filter (\(sym, position) -> sym == symbol) element) | element <- symbolMap]

symbolsOf :: Position -> SymbolsMap -> SymbolsGrid
symbolsOf position symbolMap = do
  [map fst (filter (\(symbol, pos) -> pos == position) element) | element <- symbolMap]

-- getBlankGrid :: SymbolsGrid -> SymbolsMap
-- getBlankGrid grid = do
--   positionsOf B grid
--
next :: Symbol -> Symbol
next O = X
next B = B
next X = O

empty :: SymbolsGrid
empty = replicate gridSize (replicate gridSize B)

full :: SymbolsGrid -> Bool
full = notElem B . concat

turn :: SymbolsGrid -> Symbol
turn grid = if os <= xs then O else X
  where
    ps = concat grid
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)

diag :: SymbolsGrid -> [Symbol]
diag grid = [grid !! n !! n | n <- [0 .. gridSize - 1]]

wins :: Symbol -> SymbolsGrid -> Bool
wins p grid = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = grid
    cols = transpose grid
    dias = [diag grid, diag (map reverse grid)]

won :: SymbolsGrid -> Bool
won grid = wins O grid || wins X grid

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
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

-- getNearestBlank:: Position -> [Position]
-- getNearestBlank position positions =
--
-- TODO make this
-- getTurn :: [Position] -> [SymbolsGrid] -> Int
-- getTurn positions grid = do
--   return a
--

processInput :: IO Int
processInput = do
  input <- readLine'
  return 0

prompt :: Symbol -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

getNat :: String -> IO Int
getNat prompt = do
  printMsg prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

run :: SymbolsGrid -> Symbol -> IO ()
run g p = do
  -- Reset screen
  clearScn
  goto (0, 0)
  putGrid g
  run' g p

run' :: SymbolsGrid -> Symbol -> IO ()
run' g p
  | wins O g = printMsg "Player O wins!\n"
  | wins X g = printMsg "Player X wins!\n"
  | full g = printMsg "It's a draw!\n"
  | otherwise = do
      -- TODO:  make a getNat that works with arrows and returns the index
      turn <- getNat (prompt p)
      case move g turn p of
        [] -> do
          -- printMsg pos "ERROR: Invalid move"
          run' g p
        g' -> do
          run (head g') (next p)

printMsg :: String -> IO ()
printMsg msg = do
  goto (0, 1)
  putStrLn msg

tictactoe :: IO ()
tictactoe = run empty O
