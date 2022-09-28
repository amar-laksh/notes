module Graphics (getTermSize, Seqs (..), subStrIndexes, chunkStr', getValue') where

import Data.Either
import Data.Maybe
import System.Process (readProcess)

getTermSize :: IO [String]
getTermSize = do
  size <- readProcess "tput" ["cols", "lines"] []
  return (words size)

clearScn :: IO ()
clearScn = putStr (getValue Clear)

type Pos = (Int, Int)

-- goto' :: Pos -> IO ()
-- goto' (x, y) = do
--   let r = map (\c -> if c == "0" then show x else if c == "1" then show y else c)
--   print (replace "" (getValue Goto))
--   print (replace "hello")
--
type Assoc k v = [(k, v)]

data Seqs
  = Clear
  | Goto
  deriving (Eq, Show)

type Table = Assoc Seqs String

seqs :: Table
seqs = [(Clear, "\ESC[2J"), (Goto, "\ESC[0;1H")]

getValue :: Seqs -> String
getValue k = fromMaybe "" (getValue' k seqs)

getValue' :: (Eq k) => k -> Assoc k v -> Maybe v
getValue' k t = listToMaybe [v | (k', v) <- t, k == k']

-- getMap::Int -> String -> Either Int String
-- getMap k v  = either (const k) (const k) k
--
-- replace::String -> String -> String
-- replace oldNeedle newNeedle haystack =

getSubStr' :: Int -> Int -> String -> [Char]
getSubStr' start end str = [sub | (index, sub) <- zip [0 ..] str, index >= start, index <= end]

getSubStrs' :: [[Int]] -> String -> [String]
getSubStrs' indexes haystack = [getSubStr' (head index) (last index) haystack | index <- indexes, not (null index)]

subStrIndexes :: Int -> String -> [[Int]]
subStrIndexes size haystack
  | size > 1 = [indices | l <- [0 .. length haystack - 2], indices <- replicate 1 [l .. l + size - 1], last indices < length haystack]
  | otherwise = [indices | l <- [0 .. length haystack - 2], indices <- replicate 1 [l .. l + size - 1], last indices < length haystack] ++ [[length haystack - 1]]

chunkStr' :: Int -> String -> [([Int], String)]
chunkStr' chunkSize haystack = zip indexes (getSubStrs' indexes haystack)
  where
    indexes = subStrIndexes chunkSize haystack

-- replace'::String -> String -> String
-- replace' this forThis inThis =
