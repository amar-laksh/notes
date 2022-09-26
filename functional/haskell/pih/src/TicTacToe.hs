module TicTacToe
  ( Player (..),
    toSymbol,
    toGraphicGrid,
    putGrid,
    interleaveVLines,
    mixVLines,
    getNat,
    tictactoe,
  )
where

import Chapter10 (getChar')
import Data.Char
import Data.List
import Life (clearScn, goto')

gridSize :: Int
gridSize = 3

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
toSymbol B = " "

interleave xs ys = concatMap (\(x, y) -> [x, y]) (zip xs ys)

interleaveVLines :: [Char] -> Char -> [Char]
interleaveVLines row chr = init (interleave row (replicate (length row) chr))

mixVLine :: [Char] -> [Char]
mixVLine row = "\t" ++ interleaveVLines (interleaveVLines row '|') '\t'

mixVLines :: [[Char]] -> [[Char]]
mixVLines rows = [mixVLine row | row <- rows]

addHLines' :: [Char] -> [Char]
addHLines' row = replicate (length row * 5) '-'

mixLines :: [[Char]] -> [[Char]]
mixLines grid = paddingVLines ++ init (interleave rowWithVLines (replicate gridSize (addHLines' (head rowWithVLines)))) ++ paddingVLines
  where
    paddingVLines = replicate (gridSize - 1) paddingVLine
    paddingVLine = mixVLine (replicate (length (head grid)) ' ')
    rowWithVLines = mixVLines grid

toGraphicGrid :: Grid -> [String]
toGraphicGrid grid = mixLines [concatMap toSymbol c | c <- grid]

putGrid :: Grid -> IO ()
putGrid grid = mapM_ putStrLn (toGraphicGrid grid)

validMove :: Grid -> Int -> Bool
validMove grid index = index >= 0 && index < gridSize ^ 2 && concat grid !! index == B

move :: Grid -> Int -> Player -> [Grid]
move grid index player = [chop gridSize (xs ++ [player] ++ ys) | validMove grid index]
  where
    (xs, B : ys) = splitAt index (concat grid)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

toPosition :: Int -> Int -> (Int, Int)
toPosition turn offset = (turn - (15 * (offset * gridSize)), gridSize + (offset * 2))

fromPosition :: (Int, Int) -> (Int, Int)
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

processInput :: Int -> IO (Int, (Int, Int))
processInput turn = do
  input <- getChar'
  let ofst = toOffset turn
  let pos = toPosition turn ofst
  if input == '\t'
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
      if input == '\n'
        then do
          goto' pos
          return (turn, pos)
        else do
          return (-1, pos)

getNat :: Int -> IO (Int, (Int, Int))
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

run :: Grid -> Player -> (Int, Int) -> IO ()
run g p pos = do
  clearScn
  goto' (1, 1)
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
          printMsg pos "ERROR: Invalid move"
          run' g p turn
        [g'] -> do
          run g' (next p) pos

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

printMsg :: (Int, Int) -> String -> IO ()
printMsg pos msg = do
  goto' (gridSize * gridSize * 10, 1)
  putStrLn msg
  goto' pos
