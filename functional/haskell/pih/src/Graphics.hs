module Graphics (getTermSize, replace', goto, inHead, clearScn, getCapSeq, toLineSymbol, Line (..)) where

import Data.Char
import Data.Either
import Data.List (sortBy)
import Data.Maybe
import System.Process (readProcess)

-- Reference: https://en.wikipedia.org/wiki/ANSI_escape_code
-- man 5 terminfo
-- man 1 tput
type Pos = (Int, Int)

getTermSize :: IO [String]
getTermSize = do
  size <- readProcess "tput" ["cols", "lines"] []
  return (words size)

getCapSeq :: [String] -> IO String
getCapSeq capname = do
  readProcess "tput" capname []

printCapSeq :: [String] -> IO ()
printCapSeq capname = do
  getCapSeq capname >>= \s -> putStr s

clearScn :: IO ()
clearScn = do
  getCapSeq ["clear"] >>= \s -> putStr s

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

plot :: Pos -> IO ()
plot p = do
  goto p
  putStr "*"

savePos :: IO ()
savePos = printCapSeq ["sc"]

restorePos :: IO ()
restorePos = printCapSeq ["rc"]

-- Lines generation algorithm
data Line = HLine | VLine | DLLine | DRLine deriving (Show, Eq, Ord)

toLineSymbol :: Line -> Char
toLineSymbol HLine = '-'
toLineSymbol VLine = '|'
toLineSymbol DLLine = '\\'
toLineSymbol DRLine = '/'

-- TODO put string manipulation in different module
inHead :: String -> String -> Bool
inHead needle haystack = take (length needle) haystack == needle

substring :: [Char] -> [Char] -> Bool
substring l s = check' s l True
  where
    check' _ [] h = True
    check' [] _ h = False
    check' (x : xs) (y : ys) h = (y == x && check' xs ys False) || (h && check' xs (y : ys) h)

replace' :: String -> String -> String -> String
replace' needle replacement [] = []
replace' needle replacement haystack
  | inHead needle haystack = replacement ++ replace' needle replacement (drop (length needle) haystack)
  | not (inHead needle haystack) = head haystack : replace' needle replacement (tail haystack)
