module TicTacToe
  ( Player (..),
    toSymbol,
    toGraphicGrid,
    putGrid,
    interleaveVLines,
  )
where

import Data.Char
import Data.List

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
turn g = if os <= xs then O else X
  where
    ps = concat g
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. gridSize - 1]]

winFor :: Player -> Grid -> Bool
winFor p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

won :: Grid -> Bool
won g = winFor O g || winFor X g

toSymbol :: Player -> String
toSymbol O = "⭕"
toSymbol X = "❌"
toSymbol B = " "

interleave xs ys = concatMap (\(x, y) -> [x, y]) (zip xs ys)

interleaveVLines :: [Char] -> Char -> [Char]
interleaveVLines row chr = init (interleave row (replicate (length row) chr))

mixVLines :: [[Char]] -> [[Char]]
mixVLines rows = [interleaveVLines (interleaveVLines row '|') '\t' | row <- rows]

addHLines' :: [Char] -> [Char]
addHLines' row = replicate (length row * 4) '-'

addLines :: [[Char]] -> [[Char]]
addLines g = init (interleave rowWithVLines (replicate 3 (addHLines' (head rowWithVLines))))
  where
    rowWithVLines = mixVLines g

toGraphicGrid :: Grid -> [String]
toGraphicGrid g = addLines [concatMap toSymbol c | c <- g]

putGrid :: Grid -> IO ()
putGrid g = mapM_ putStrLn (toGraphicGrid g)
