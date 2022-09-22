module Chapter10
  ( putStr',
    adder',
  )
where

import Data.Maybe
import Text.Read

putStr' :: String -> IO ()
putStr' str = sequence_ [putChar chr | chr <- str]

getInt :: IO Int
getInt = do
  x <- getLine
  -- Ignoring anything that's not a number
  let ans = fromMaybe 0 (readMaybe x :: Maybe Int)
  return ans

readInts :: Int -> IO [Int]
readInts 0 = return []
readInts n = do
  x <- getInt
  xs <- readInts (n - 1)
  return (x : xs)

adder' :: IO ()
adder' = do
  putStrLn "How many numbers? "
  n <- getInt
  xs <- readInts n
  putStr "The total is: "
  print (sum xs)
